# Shared measurements and spatial units for the interactive viewer and exports.
# Do not subset SFE images with [, j]: some SFE releases crop or realize them.
app_spot_indices <- function(spe, sample) {
    i <- which(spe$sample_id == sample)
    if ("exclude_overlapping" %in% colnames(colData(spe))) {
        i <- i[!is.na(spe$exclude_overlapping[i]) & !spe$exclude_overlapping[i]]
    }
    i
}

app_spot_coordinates <- function(spe, i) {
    xy <- SpatialExperiment::spatialCoords(spe)
    preferred <- list(c("pxl_col_in_fullres", "pxl_row_in_fullres"),
        c("x_centroid", "y_centroid"), c("x", "y"))
    use <- Filter(function(n) all(n %in% colnames(xy)), preferred)
    if (length(use)) xy <- xy[, use[[1]], drop = FALSE]
    if (ncol(xy) < 2L) stop("Two spatial coordinate columns are required.", call. = FALSE)
    xy <- xy[i, 1:2, drop = FALSE]
    if (any(!is.finite(xy))) stop("Spatial coordinates must be finite.", call. = FALSE)
    unname(xy)
}

app_spot_y_down <- function(spe, source) {
    if (!is.null(source$y_direction)) return(source$y_direction == "down")
    !methods::is(spe, "SpatialFeatureExperiment") &&
        all(c("pxl_col_in_fullres", "pxl_row_in_fullres") %in% colnames(spatialCoords(spe)))
}

app_spot_geometry <- function(spe, i) {
    if (!methods::is(spe, "SpatialFeatureExperiment")) return(NULL)
    if (!requireNamespace("SpatialFeatureExperiment", quietly = TRUE) ||
        !requireNamespace("sf", quietly = TRUE)) {
        stop("SpatialFeatureExperiment and sf are required to display SFE geometries.", call. = FALSE)
    }
    geometries <- SpatialFeatureExperiment::colGeometries(spe)
    # Inspect the preferred column first. Classifying every geometry column can
    # otherwise traverse all cells, including unused centroid geometries.
    candidates <- intersect(unique(c("cellSeg", "spotPoly", names(geometries))), names(geometries))
    name <- NULL
    for (candidate in candidates) {
        geometry <- sf::st_geometry(geometries[[candidate]])
        polygonal <- inherits(geometry, "sfc_POLYGON") || inherits(geometry, "sfc_MULTIPOLYGON")
        if (!polygonal && inherits(geometry, "sfc_GEOMETRY")) {
            polygonal <- all(as.character(sf::st_geometry_type(geometry)) %in% c("POLYGON", "MULTIPOLYGON"))
        }
        if (polygonal) { name <- candidate; break }
    }
    if (is.null(name)) return(NULL)
    g <- geometries[[name]][i, , drop = FALSE]
    if (any(sf::st_is_empty(g)) || any(!sf::st_is_valid(g))) {
        stop("The selected column geometry contains empty or invalid boundaries: ", name, call. = FALSE)
    }
    # Only column geometries are eligible for expression coloring. Annotation
    # geometries need not correspond to columns of the expression matrix.
    sf::st_sf(unit_index = seq_along(i) - 1L, geometry = sf::st_geometry(g))
}

# Private little-endian boundary format: eight uint32 header words, ring vertex
# offsets, polygon ring offsets, polygon unit IDs, ring unit IDs, padding to an
# eight-byte boundary, then interleaved float64 XY. Preserve coordinate precision
# and polygon/hole membership while avoiding expensive GeoJSON text formatting.
app_spot_write_geometry <- function(g, path) {
    geometry <- sf::st_geometry(g)
    counts <- vapply(geometry, function(unit) if (inherits(unit, "POLYGON")) 1L else length(unit), integer(1))
    polygons <- unlist(lapply(geometry, function(unit) {
        if (inherits(unit, "POLYGON")) list(unclass(unit)) else unclass(unit)
    }), recursive = FALSE)
    ring_counts <- lengths(polygons)
    rings <- unlist(polygons, recursive = FALSE)
    dimensions <- vapply(rings, function(ring) dim(ring)[2L], integer(1))
    vertices <- lengths(rings) %/% dimensions
    polygon_units <- rep.int(g$unit_index, counts)
    ring_units <- rep.int(polygon_units, ring_counts)
    ring_offsets <- c(0, cumsum(vertices))
    polygon_offsets <- c(0, cumsum(ring_counts))
    # Flatten in one native operation, then interleave XY with vectorized indices.
    # A separate transpose/subset for each cell is costly on large datasets.
    coordinates <- unlist(rings, use.names = FALSE)
    scalar_offsets <- c(0, cumsum(lengths(rings)))
    x_index <- seq_len(sum(vertices)) + rep.int(utils::head(scalar_offsets, -1L) - utils::head(ring_offsets, -1L), vertices)
    xy <- as.vector(rbind(coordinates[x_index], coordinates[x_index + rep.int(vertices, vertices)]))
    header <- c(1397506628L, 1L, length(rings), sum(vertices), length(polygons), 8L, nrow(g), 0L)
    stopifnot(all(header <= .Machine$integer.max), length(xy) == 2 * sum(vertices))
    indices <- as.integer(c(header, ring_offsets, polygon_offsets, polygon_units, ring_units))
    if (length(indices) %% 2L) indices <- c(indices, 0L)
    con <- file(path, "wb")
    tryCatch({
        writeBin(indices, con, size = 4L, endian = "little")
        writeBin(xy, con, size = 8L, endian = "little")
    }, finally = close(con))
    invisible(path)
}

app_spot_values <- function(spe, i, gene) {
    ids <- unique(gene$geneid)
    valid <- ids %in% c(colnames(colData(spe)), rowData(spe)$gene_search, rownames(spe))
    if (any(!valid)) stop("Unknown continuous feature: ", paste(ids[!valid], collapse = ", "), call. = FALSE)
    columns <- ids[ids %in% colnames(colData(spe))]
    cd <- colData(spe)[i, columns, drop = FALSE]
    if (!all(vapply(cd, is.numeric, logical(1)))) stop("Continuous metadata must be numeric.", call. = FALSE)
    genes <- setdiff(ids, columns)
    rows <- unique(c(match(genes, rowData(spe)$gene_search), match(genes, rownames(spe))))
    rows <- rows[!is.na(rows)]
    if (!gene$assayname %in% SummarizedExperiment::assayNames(spe)) stop("Unknown assay.", call. = FALSE)
    # Subset the assay, not the SFE. Only selected features are materialized.
    mat <- cbind(as.matrix(cd), t(as.matrix(SummarizedExperiment::assay(spe, gene$assayname)[rows, i, drop = FALSE])))
    if (!ncol(mat)) stop("Select a continuous feature.", call. = FALSE)
    value <- if (ncol(mat) == 1L) mat[, 1] else switch(gene$multi_gene_method,
        z_score = multi_gene_z_score(mat), pca = multi_gene_pca(mat),
        sparsity = multi_gene_sparsity(mat), stop("Unknown combination method.", call. = FALSE))
    # Match vis_gene(): score the sample, cap using its empirical order statistic,
    # then mask values <= the threshold. Cluster filters do not refit the score.
    cap <- gene$cap_percentile
    if (!is.finite(cap) || cap <= 0 || cap > 1) stop("Maximum percentile must be in (0, 1].", call. = FALSE)
    if (cap < 1) {
        sorted <- sort(value[is.finite(value)])
        if (length(sorted)) value <- pmin(value, sorted[max(1L, round(length(sorted) * cap))])
    }
    value[!is.finite(value) | value <= gene$minCount] <- NA_real_
    as.numeric(value)
}

app_spot_frame <- function(spe, sample, cluster, gene, clusters = NULL) {
    i <- app_spot_indices(spe, sample)
    if (!length(i)) stop("No spatial units available for this sample.", call. = FALSE)
    result <- app_capture_warnings(app_spot_values(spe, i, gene))
    labels <- as.character(spe[[cluster]][i])
    labels[is.na(labels)] <- "(missing)"
    eligible <- is.finite(result$p)
    if (!is.null(clusters)) eligible <- eligible & labels %in% clusters
    xy <- app_spot_coordinates(spe, i)
    list(i = i, key = spe$key[i], x = xy[, 1], y = xy[, 2],
        value = result$p, labels = labels, eligible = eligible, warnings = result$warnings)
}

app_spot_domain <- function(value) {
    value <- value[is.finite(value)]
    if (!length(value)) return(c(0, 1))
    r <- range(value)
    if (r[1] == r[2]) r <- r + c(-0.5, 0.5)
    r
}

app_spot_rgba <- function(colors) {
    unname(grDevices::col2rgb(colors, alpha = TRUE))
}

# Session files bypass websocket size limits and keep geometry/positions cached
# while only feature colors and masks change. Every resource is session scoped.
app_viewer_resources <- function(session) {
    root <- tempfile("spatialLIBD-viewer-")
    dir.create(root)
    prefix <- paste0("spatiallibd-", session$token)
    shiny::addResourcePath(prefix, root)
    mounts <- character()
    cache <- new.env(parent = emptyenv())
    session$onSessionEnded(function() {
        shiny::removeResourcePath(prefix)
        for (p in mounts) shiny::removeResourcePath(p)
        unlink(root, recursive = TRUE)
    })
    asset <- function(path) {
        path <- normalizePath(path, mustWork = TRUE)
        key <- paste0("asset:", path)
        if (exists(key, cache, inherits = FALSE)) return(cache[[key]])
        if (!dir.exists(path) && grepl("\\.tiff?$", path, ignore.case = TRUE)) {
            # Shiny's static file handler does not honor Range. geotiff.js must
            # receive only the requested IFD/tile bytes, never the entire slide.
            url <- session$registerDataObj(paste0("pyramid-", length(ls(cache)), ".ome.tif"),
                path, function(data, req) {
                    size <- unname(file.info(data)$size)
                    headers <- list("Content-Type" = "image/tiff", "Accept-Ranges" = "bytes")
                    if (identical(req$REQUEST_METHOD, "HEAD")) {
                        headers[["Content-Length"]] <- as.character(size)
                        return(list(status = 200L, headers = headers, body = raw()))
                    }
                    range <- req$HTTP_RANGE
                    invalid <- function() {
                        headers[["Content-Range"]] <- paste0("bytes */", size)
                        list(status = 416L, headers = headers, body = "A valid byte range is required.")
                    }
                    if (is.null(range) || identical(range, "bytes=-") || !grepl("^bytes=[0-9]*-[0-9]*$", range)) return(invalid())
                    bounds <- strsplit(sub("^bytes=", "", range), "-", fixed = TRUE)[[1]]
                    if (!nzchar(bounds[1])) {
                        n <- suppressWarnings(as.numeric(bounds[2]))
                        start <- max(0, size - n)
                        end <- size - 1
                    } else {
                        start <- suppressWarnings(as.numeric(bounds[1]))
                        end <- if (length(bounds) < 2L || !nzchar(bounds[2])) size - 1 else suppressWarnings(as.numeric(bounds[2]))
                        end <- min(end, size - 1)
                    }
                    n <- end - start + 1
                    if (any(!is.finite(c(start, end, n))) || start < 0 || n <= 0 ||
                        start >= size || n > .Machine$integer.max) return(invalid())
                    con <- file(data, "rb")
                    on.exit(close(con))
                    seek(con, start, origin = "start")
                    body <- readBin(con, "raw", n = n)
                    headers[["Content-Range"]] <- paste0("bytes ", start, "-", end, "/", size)
                    headers[["Content-Length"]] <- as.character(length(body))
                    list(status = 206L, headers = headers, body = body)
                })
            cache[[key]] <- url
            return(url)
        }
        # Mount the exact directory required by an image, never its parent tree.
        mount <- paste0(prefix, "-image-", length(mounts) + 1L)
        is_dir <- dir.exists(path)
        shiny::addResourcePath(mount, if (is_dir) path else dirname(path))
        mounts <<- c(mounts, mount)
        url <- paste0(mount, if (is_dir) "/" else paste0("/", utils::URLencode(basename(path), reserved = TRUE)))
        cache[[key]] <- url
        url
    }
    list(root = root, prefix = prefix, cache = cache, asset = asset,
        url = function(path) paste0(prefix, "/", basename(path)))
}
