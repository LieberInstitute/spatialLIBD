# Discover external pyramids automatically; no new run_app image-path argument.
app_image_catalog <- function(spe) {
    registered <- S4Vectors::metadata(spe)$viewer_images
    images <- SpatialExperiment::imgData(spe)
    result <- setNames(vector("list", length(unique(spe$sample_id))), unique(spe$sample_id))
    anchors <- unlist(lapply(images$data, function(im) {
        tryCatch(SpatialExperiment::imgSource(im), error = function(e) NULL)
    }), use.names = FALSE)
    anchors <- anchors[!grepl("^https?://", anchors)]
    resolve <- function(path) {
        if (is.null(path)) return(NULL)
        if (grepl("^https?://", path)) return(path)
        candidates <- c(path, unlist(lapply(anchors, function(a) {
            c(file.path(dirname(a), path), file.path(dirname(dirname(a)), path))
        })))
        found <- candidates[file.exists(candidates)]
        if (!length(found)) stop("Cannot locate registered image source: ", path, call. = FALSE)
        normalizePath(found[1], mustWork = TRUE)
    }
    for (sample in names(result)) {
        entry <- Filter(function(x) identical(as.character(x$sample_id), sample), registered)
        sources <- if (length(entry)) entry[[1]]$sources else list()
        extent <- if (length(entry)) entry[[1]]$extent_pixels else NULL
        ids <- vapply(sources, function(x) x$image_id, character(1))
        for (s in seq_along(sources)) {
            sources[[s]]$path <- resolve(sources[[s]]$path)
            if (is.null(sources[[s]]$extent)) sources[[s]]$extent <- extent
        }
        rows <- which(images$sample_id == sample)
        scene_direction <- if (length(sources)) sources[[1]]$y_direction else NULL
        for (j in rows) {
            id <- images$image_id[j]
            if (id %in% ids) next
            im <- images$data[[j]]
            path <- tryCatch(SpatialExperiment::imgSource(im), error = function(e) NULL)
            # Loaded images have no source path; never realize a stored full-size
            # TIFF to decide whether it can be served as a pyramid.
            type <- if (length(path) == 1L && grepl("\\.ome\\.tiff?($|\\?)", path, ignore.case = TRUE)) "ome-tiff" else "image"
            native_extent <- NULL
            if (methods::is(im, "SpatRasterImage") && requireNamespace("SpatialFeatureExperiment", quietly = TRUE)) {
                b <- as.vector(SpatialFeatureExperiment::ext(im))
                native_extent <- b[c(1, 3, 2, 4)]
            }
            sources[[length(sources) + 1L]] <- list(image_id = id, type = type,
                path = if (type == "ome-tiff") resolve(path) else NULL,
                scale_factor = images$scaleFactor[j], image_row = j,
                extent = if (is.null(native_extent)) extent else native_extent,
                y_direction = if (!is.null(scene_direction)) scene_direction else
                    if (app_spot_is_sfe(spe)) "up" else NULL)
        }
        if (length(sources)) {
            names(sources) <- vapply(sources, function(x) x$image_id, character(1))
            default <- if (length(entry)) entry[[1]]$default_image_id else names(sources)[1]
            if (is.null(default) || !default %in% names(sources)) default <- names(sources)[1]
            result[[sample]] <- list(sources = sources, default = default, extent = extent)
        }
    }
    result
}

app_image_choices <- function(catalog, type = NULL) {
    unique(unlist(lapply(catalog, function(s) {
        sources <- s$sources
        if (!is.null(type)) sources <- Filter(function(im) im$type %in% type, sources)
        names(sources)
    }), use.names = FALSE))
}

app_image_source <- function(catalog, sample, id = NULL, thumbnail = FALSE) {
    if (identical(id, "")) return(NULL)
    entry <- catalog[[sample]]
    if (is.null(entry) || !length(entry$sources)) return(NULL)
    if (is.null(id) || !id %in% names(entry$sources)) id <- entry$default
    source <- entry$sources[[id]]
    if (thumbnail && source$type != "image") {
        # Full-size sources are never materialized for static grids or editing.
        choices <- Filter(function(s) s$type == "image", entry$sources)
        if (!length(choices)) return(NULL)
        source <- choices[[app_preferred(names(choices), "lowres")]]
    }
    source
}

app_image_raster <- function(spe, sample, source) {
    if (is.null(source)) return(NULL)
    if (source$type != "image") stop("A low-resolution image is required for this operation.", call. = FALSE)
    if (!is.null(source$path)) {
        image <- as.raster(magick::image_read(source$path))
    } else {
        image <- SpatialExperiment::imgRaster(spe, sample_id = sample, image_id = source$image_id)
    }
    scale <- source$scale_factor
    extent <- source$extent
    if (is.null(extent) && (is.null(scale) || !is.finite(scale) || scale <= 0)) {
        stop("Image registration requires a positive scale factor: ", source$image_id, call. = FALSE)
    }
    if (is.null(extent)) extent <- c(0, 0, ncol(image) / scale, nrow(image) / scale)
    list(image = image, extent = extent)
}

app_image_browser <- function(spe, sample, source, resources) {
    if (is.null(source)) return(NULL)
    if (source$type == "image") {
        key <- paste("raster", sample, source$image_id,
            if (source$image_id == "edited_imaged") S4Vectors::metadata(spe)$app_image_revision else 0, sep = ":")
        # Edited pixels have a different source object after Update; the caller
        # invalidates this cache when an edited variant is applied.
        if (!exists(key, resources$cache, inherits = FALSE)) {
            raster <- app_image_raster(spe, sample, source)
            path <- tempfile("image-", resources$root, ".png")
            magick::image_write(magick::image_read(raster$image), path, format = "png")
            resources$cache[[key]] <- list(type = "image", url = resources$url(path), extent = raster$extent)
        }
        return(resources$cache[[key]])
    }
    url <- if (grepl("^https?://", source$path)) source$path else resources$asset(source$path)
    list(type = source$type, url = url, extent = source$extent,
        pixelSize = source$pixel_size_um_xy, levelScales = source$level_scale_factors,
        origin = source$origin_pixels, yDirection = source$y_direction)
}
