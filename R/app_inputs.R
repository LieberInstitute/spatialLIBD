#' Prepare disk-backed inputs for the spatialLIBD app
#'
#' Convert an ordinary SPE/SFE once, before deployment. Assays retain their
#' values in gene-oriented BPCells directories; polygons and images are stored
#' separately from a lightweight SpatialExperiment. Input objects are unchanged.
#'
#' @param spe A SpatialExperiment or SpatialFeatureExperiment with sample_id,
#'   feature identifiers, spatial coordinates, and at least one assay.
#' @param output_dir A new directory. Existing directories are never overwritten.
#' @param fullres_images Optional named list indexed by sample_id. Each entry is
#'   a path to an aligned OME-TIFF/OME-Zarr pyramid, or a descriptor with path,
#'   image_id (default "fullres"), type, extent (xmin, ymin, xmax, ymax in spatial
#'   coordinate units), and y_direction ("up"/"down"). Descriptors may also supply
#'   pixel_size_um_xy. Multiple sources can be supplied as list(sources = ...).
#'   Paths without registration use the experiment's image coordinate frame.
#'   Images must already be oriented to that frame; preparation does not rotate
#'   or reflect full-resolution pixels. Local pyramids are copied into the output.
#' @return Invisibly, the normalized output directory. Use open_app_dataset()
#'   to open it, then pass the result to run_app().
#' @export
prepare_app_inputs <- function(spe, output_dir, fullres_images = NULL) {
    if (!methods::is(spe, "SpatialExperiment")) stop("spe must be an SPE or SFE.", call. = FALSE)
    if (!length(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
        stop("Provide one output directory.", call. = FALSE)
    }
    if (file.exists(output_dir)) stop("output_dir already exists; choose a new directory.", call. = FALSE)
    # Normalize feature and unit identities before creating any backing files.
    cd <- SummarizedExperiment::colData(spe)
    rd <- SummarizedExperiment::rowData(spe)
    samples <- as.character(cd$sample_id)
    if (length(samples) != ncol(spe) || !length(samples) || anyNA(samples) || any(!nzchar(samples))) {
        stop("Every cell must have a nonempty sample_id.", call. = FALSE)
    }
    cd$sample_id <- samples
    features <- if ("gene_id" %in% names(rd)) as.character(rd$gene_id) else rownames(spe)
    if (length(features) != nrow(spe) || !length(features) || anyNA(features) ||
        any(!nzchar(features)) || anyDuplicated(features)) {
        stop("Provide unique, nonempty feature IDs in rownames or gene_id.", call. = FALSE)
    }
    rd$gene_id <- features
    if (!"gene_name" %in% names(rd)) {
        name_column <- intersect(c("symbol", "gene_symbol", "name"), names(rd))
        rd$gene_name <- if (length(name_column)) rd[[name_column[1L]]] else features
    }
    rd$gene_name <- as.character(rd$gene_name)
    missing <- is.na(rd$gene_name) | !nzchar(rd$gene_name)
    rd$gene_name[missing] <- features[missing]
    rd$gene_search <- paste0(rd$gene_name, "; ", features)
    rownames(rd) <- features
    cells <- colnames(spe)
    if (is.null(cells)) cells <- paste0("unit", seq_len(ncol(spe)))
    keys <- if ("key" %in% names(cd)) as.character(cd$key) else paste0(cells, "_", samples)
    if (anyNA(keys) || any(!nzchar(keys)) || anyDuplicated(keys)) stop("Cell keys must be unique and nonempty.", call. = FALSE)
    if (anyDuplicated(paste(samples, cells, sep = "\034"))) stop("Cell names must be unique within each sample.", call. = FALSE)
    cd$key <- keys
    if (!"ManualAnnotation" %in% names(cd)) cd$ManualAnnotation <- rep("NA", ncol(spe))
    assay_names <- SummarizedExperiment::assayNames(spe)
    if (!length(assay_names) || anyNA(assay_names) || any(!nzchar(assay_names)) || anyDuplicated(assay_names)) {
        stop("Provide at least one uniquely named assay.", call. = FALSE)
    }
    # Construct rather than subset/coerce SFE: its subsetting can realize images.
    shell <- SpatialExperiment::SpatialExperiment(rowData = rd, colData = cd,
        spatialCoords = SpatialExperiment::spatialCoords(spe),
        reducedDims = SingleCellExperiment::reducedDims(spe))
    colnames(shell) <- cells
    for (sample in unique(samples)) app_spot_coordinates(shell, which(samples == sample))
    catalog <- app_prepare_image_catalog(spe, fullres_images)
    # Stage the complete dataset beside its destination; failures remove only staged files.
    parent <- dirname(output_dir)
    if (!dir.exists(parent)) stop("The parent of output_dir must exist.", call. = FALSE)
    stage <- tempfile(".spatialLIBD-prepare-", parent)
    dir.create(stage)
    on.exit(unlink(stage, recursive = TRUE), add = TRUE)
    for (dir in c("assays", "boundaries", "images")) dir.create(file.path(stage, dir))
    assay_paths <- character(length(assay_names))
    names(assay_paths) <- assay_names
    # Arrange assays on disk for gene-wise reads without materializing the full matrix.
    for (j in seq_along(assay_names)) {
        message("Preparing assay ", assay_names[j], " (gene-oriented, double precision)")
        a <- SummarizedExperiment::assay(spe, assay_names[j], withDimnames = FALSE)
        # BPCells may wrap dgCMatrix directly; other delayed inputs are streamed
        # through the cell-oriented directory before changing physical order.
        path <- file.path("assays", paste0("assay-", j))
        if (inherits(a, "sparseMatrix")) {
            a <- methods::as(a, "dgCMatrix")
            a <- Matrix::t(a)
            dimnames(a) <- NULL
            BPCells::write_matrix_dir(BPCells::convert_matrix_type(methods::as(a, "IterableMatrix"), "double"), file.path(stage, path))
        } else {
            cell_path <- file.path(stage, paste0(".cell-assay-", j))
            mat <- if (methods::is(a, "IterableMatrix")) a else app_bp_stream(a, cell_path)
            if (!identical(BPCells::storage_order(mat), "row")) {
                mat <- BPCells::transpose_storage_order(mat, outdir = file.path(stage, paste0(".row-assay-", j)))
            }
            BPCells::write_matrix_dir(BPCells::t(BPCells::convert_matrix_type(mat, "double")), file.path(stage, path))
            unlink(c(cell_path, file.path(stage, paste0(".row-assay-", j))), recursive = TRUE)
        }
        assay_paths[j] <- path
        rm(a); gc(verbose = FALSE)
    }
    # Serialize boundaries per sample using the same unit order as its expression values.
    message("Preparing sample boundaries")
    boundaries <- setNames(vector("list", length(unique(samples))), unique(samples))
    for (j in seq_along(boundaries)) {
        sample <- names(boundaries)[j]
        g <- app_spot_geometry(spe, app_spot_indices(shell, sample))
        if (!is.null(g)) {
            path <- file.path("boundaries", paste0("sample-", j, ".bin"))
            app_spot_write_geometry(g, file.path(stage, path))
            boundaries[[j]] <- path
        }
    }
    # Copy image assets with relative paths so the prepared folder can be relocated intact.
    message("Copying registered image assets")
    assets <- character()
    for (j in seq_along(catalog)) for (k in seq_along(catalog[[j]]$sources)) {
        source <- catalog[[j]]$sources[[k]]
        stem <- paste0("sample-", j, "-image-", k)
        if (source$type == "image") {
            raster <- app_image_raster(spe, names(catalog)[j], source)
            if (is.null(source$scale_factor) || !is.finite(source$scale_factor)) source$scale_factor <- 1
            relative <- file.path("images", paste0(stem, ".png"))
            magick::image_write(magick::image_read(raster$image), file.path(stage, relative), format = "png")
            source$extent <- raster$extent
        } else {
            relative <- file.path("images", paste0(stem, if (source$type == "ome-zarr") ".ome.zarr" else ".ome.tif"))
            destination <- file.path(stage, relative)
            if (dir.exists(source$path)) {
                dir.create(destination)
                ok <- file.copy(list.files(source$path, full.names = TRUE, all.files = TRUE, no.. = TRUE), destination, recursive = TRUE)
            } else ok <- file.copy(source$path, destination)
            if (!length(ok) || !all(ok)) stop("Could not copy image: ", source$path, call. = FALSE)
        }
        assets <- c(assets, relative)
        source$path <- relative
        source$image_row <- NULL
        source$companion_ome_tiff <- NULL
        catalog[[j]]$sources[[k]] <- source
    }
    # Publish only after the experiment, assets, and manifest have all been written.
    S4Vectors::metadata(shell) <- list(app_is_sfe = methods::is(spe, "SpatialFeatureExperiment"))
    saveRDS(shell, file.path(stage, "experiment.rds"), compress = FALSE)
    manifest <- list(format = "spatialLIBD-app", version = 1L,
        experiment = "experiment.rds", assays = assay_paths, catalog = catalog,
        boundaries = boundaries, assets = assets, dimensions = dim(shell))
    saveRDS(manifest, file.path(stage, "manifest.rds"), compress = FALSE)
    if (file.exists(output_dir) || !file.rename(stage, output_dir)) stop("Could not publish prepared inputs.", call. = FALSE)
    invisible(normalizePath(output_dir))
}

# Stream delayed/dense assay columns into bounded temporary BPCells blocks.
app_bp_stream <- function(a, path) {
    blocks <- character()
    on.exit(unlink(blocks, recursive = TRUE), add = TRUE)
    step <- max(1L, floor(32 * 1024^2 / (8 * nrow(a))))
    for (start in seq.int(1L, ncol(a), by = step)) {
        b <- as.matrix(a[, seq.int(start, min(ncol(a), start + step - 1L)), drop = FALSE])
        b <- methods::as(b, "dgCMatrix")
        dimnames(b) <- NULL
        block <- paste0(path, "-", start)
        BPCells::write_matrix_dir(methods::as(b, "IterableMatrix"), block)
        blocks <- c(blocks, block)
    }
    mat <- Reduce(function(x, y) methods::cbind2(x, y), lapply(blocks, BPCells::open_matrix_dir))
    BPCells::write_matrix_dir(mat, path)
}

#' Open prepared spatialLIBD app inputs
#'
#' @param path Directory written by prepare_app_inputs(). Its complete contents
#'   can be moved together; individual backing files must not be renamed.
#' @return A spatialLIBD_app_dataset containing a lightweight experiment with
#'   lazy, gene-oriented BPCells assays. Pass this object to run_app().
#' @export
open_app_dataset <- function(path) {
    path <- normalizePath(path, mustWork = TRUE)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    if (!identical(manifest$format, "spatialLIBD-app") || !identical(manifest$version, 1L)) {
        stop("Unsupported prepared app dataset format/version.", call. = FALSE)
    }
    # Reject absolute paths and parent traversal in the portable dataset manifest.
    resolve <- function(p) {
        if (length(p) != 1L || is.na(p) || grepl("^(/|[A-Za-z]:)|(^|[/\\\\])\\.\\.([/\\\\]|$)", p)) {
            stop("Invalid relative path in prepared inputs.", call. = FALSE)
        }
        normalizePath(file.path(path, p), mustWork = TRUE)
    }
    invisible(lapply(c(manifest$assets, unlist(manifest$boundaries)), resolve))
    spe <- readRDS(resolve(manifest$experiment))
    if (!methods::is(spe, "SpatialExperiment") || !identical(dim(spe), manifest$dimensions)) {
        stop("Prepared experiment dimensions do not match the manifest.", call. = FALSE)
    }
    # Reopen assays lazily and verify their physical order and logical dimensions.
    assays <- lapply(manifest$assays, function(p) {
        a <- BPCells::t(BPCells::open_matrix_dir(resolve(p)))
        if (!identical(dim(a), dim(spe)) || !identical(BPCells::storage_order(a), "row")) {
            stop("Prepared assay dimensions/orientation do not match the experiment.", call. = FALSE)
        }
        a
    })
    SummarizedExperiment::assays(spe, withDimnames = FALSE) <- assays
    # Restore absolute asset locations relative to the dataset's current directory.
    catalog <- manifest$catalog
    for (j in seq_along(catalog)) for (k in seq_along(catalog[[j]]$sources)) {
        catalog[[j]]$sources[[k]]$path <- resolve(catalog[[j]]$sources[[k]]$path)
    }
    # StoredSpatialImage references preserve ordinary SPE image operations and
    # the existing session-local low-resolution editor without decoding pixels.
    images <- list()
    for (sample in names(catalog)) for (source in catalog[[sample]]$sources) {
        if (source$type == "image") images[[length(images) + 1L]] <- S4Vectors::DataFrame(
            sample_id = sample, image_id = source$image_id,
            data = I(list(SpatialExperiment::SpatialImage(source$path))),
            scaleFactor = source$scale_factor)
    }
    if (length(images)) SpatialExperiment::imgData(spe) <- do.call(rbind, images)
    # Reattach viewer registration and boundary metadata without decoding full-size images.
    registered <- lapply(names(catalog), function(sample) list(sample_id = sample,
        default_image_id = catalog[[sample]]$default, extent_pixels = catalog[[sample]]$extent,
        sources = unname(catalog[[sample]]$sources)))
    S4Vectors::metadata(spe)$viewer_images <- registered
    S4Vectors::metadata(spe)$app_boundaries <- lapply(manifest$boundaries, function(p) if (is.null(p)) NULL else resolve(p))
    S4Vectors::metadata(spe)$app_prepared <- TRUE
    structure(list(spe = spe, path = path), class = "spatialLIBD_app_dataset")
}

# Merge supplied pyramids with existing images and validate their shared registration.
app_prepare_image_catalog <- function(spe, fullres_images) {
    catalog <- app_image_catalog(spe)
    # Accept one or multiple pyramids per sample, inheriting low-resolution registration.
    if (!is.null(fullres_images)) {
        if (is.character(fullres_images)) fullres_images <- as.list(fullres_images)
        if (is.null(names(fullres_images)) || anyNA(names(fullres_images)) ||
            any(!nzchar(names(fullres_images))) || anyDuplicated(names(fullres_images)) ||
            any(!names(fullres_images) %in% names(catalog))) {
            stop("fullres_images must be named by sample_id.", call. = FALSE)
        }
        for (sample in names(fullres_images)) {
            entry <- fullres_images[[sample]]
            sources <- if (is.list(entry) && !is.null(entry$sources)) entry$sources else list(entry)
            supplied_ids <- character()
            for (source in sources) {
                if (is.character(source) && length(source) == 1L) source <- list(path = source)
                if (!is.list(source) || length(source$path) != 1L || is.na(source$path)) stop("Each image needs a path.", call. = FALSE)
                if (is.null(source$image_id)) source$image_id <- "fullres"
                if (is.null(source$type)) source$type <- if (dir.exists(source$path)) "ome-zarr" else "ome-tiff"
                if (length(source$image_id) != 1L || is.na(source$image_id) || !nzchar(source$image_id) ||
                    length(source$type) != 1L || !source$type %in% c("ome-tiff", "ome-zarr")) {
                    stop("Full-resolution sources need one image_id and type ome-tiff or ome-zarr.", call. = FALSE)
                }
                if (source$image_id %in% supplied_ids) stop("Duplicate full-resolution image_id for sample: ", sample, call. = FALSE)
                supplied_ids <- c(supplied_ids, source$image_id)
                if (is.null(catalog[[sample]])) catalog[[sample]] <- list(sources = list(), default = source$image_id, extent = NULL)
                if (is.null(source$extent) && length(catalog[[sample]]$sources)) {
                    low <- app_image_source(catalog, sample, thumbnail = TRUE)
                    if (!is.null(low) && low$type == "image") source$extent <- app_image_raster(spe, sample, low)$extent
                }
                if (is.null(source$y_direction)) source$y_direction <- if (app_spot_y_down(spe, source)) "down" else "up"
                # All images in one sample use one coordinate frame.
                for (k in seq_along(catalog[[sample]]$sources)) catalog[[sample]]$sources[[k]]$y_direction <- source$y_direction
                catalog[[sample]]$sources[[source$image_id]] <- source
                catalog[[sample]]$default <- source$image_id
            }
        }
    }
    # Check every merged source before any prepared dataset is written.
    for (sample in names(catalog)) for (source in catalog[[sample]]$sources) {
        if (!source$type %in% c("image", "ome-tiff", "ome-zarr") ||
            length(source$image_id) != 1L || is.na(source$image_id) || !nzchar(source$image_id)) stop("Invalid image type/ID.", call. = FALSE)
        if (!is.null(source$extent) && (length(source$extent) != 4L || any(!is.finite(source$extent)) ||
            source$extent[3] <= source$extent[1] || source$extent[4] <= source$extent[2])) stop("Invalid image extent.", call. = FALSE)
        if (!is.null(source$y_direction) && !source$y_direction %in% c("up", "down")) stop("Invalid image y_direction.", call. = FALSE)
        if (source$type != "image" && (is.null(source$path) || !file.exists(source$path))) stop("Full-resolution image source is missing.", call. = FALSE)
    }
    catalog
}
