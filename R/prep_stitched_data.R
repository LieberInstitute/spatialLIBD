#' Prepare stitched data for plotting
#'
#' Given a \code{SpatialExperiment} built with \code{visiumStitched::build_spe()}
#' <http://research.libd.org/visiumStitched/reference/build_spe.html>, drop
#' excluded spots (specified by \code{spe$exclude_overlapping}) and compute an
#' appropriate spot size for plotting with \code{vis_gene()} or
#' \code{vis_clus()}, assuming the plot will be written to a PDF of default
#' dimensions (i.e. \code{width = 7} and \code{height = 7}).
#'
#' @param spe A \code{SpatialExperiment} built with
#' \code{visiumStitched::build_spe()}, containing a logical
#' \code{spe$exclude_overlapping} column specifying which spots to display in
#' plots
#' @inheritParams vis_clus
#'
#' @return A list with names \code{spe} and \code{point_size} containing a
#' filtered, ready-to-plot \code{SpatialExperiment} and an appropriate spot size
#' (passed to \code{vis_gene()} or \code{vis_clus()}), respectively
#'
#' @author Nicholas J. Eagles
#' @keywords internal
prep_stitched_data <- function(spe, point_size, image_id) {
    #   State assumptions about columns expected to be in the colData
    expected_cols <- c("array_row", "array_col", "exclude_overlapping")
    if (!all(expected_cols %in% colnames(colData(spe)))) {
        stop(
            sprintf(
                'Missing at least one of the following colData columns: "%s"',
                paste(expected_cols, collapse = '", "')
            ),
            call. = FALSE
        )
    }

    if (any(is.na(spe$exclude_overlapping))) {
        stop("spe$exclude_overlapping must not have NAs", call. = FALSE)
    }

    #   Drop excluded spots; verify some spots are not excluded
    subset_cols <- !spe$exclude_overlapping
    if (length(which(subset_cols)) == 0) {
        stop(
            "spe$exclude_overlapping must include some FALSE values to plot",
            call. = FALSE
        )
    }
    spe <- spe[, subset_cols]

    #   Compute an appropriate spot size for this sample

    #   Determine some pixel values for the horizontal bounds of the spots
    MIN_COL <- min(spatialCoords(spe)[, "pxl_row_in_fullres"])
    MAX_COL <- max(spatialCoords(spe)[, "pxl_row_in_fullres"])

    #   The distance between spots (in pixels) is double the average distance
    #   between array columns
    INTER_SPOT_DIST_PX <- 2 * (MAX_COL - MIN_COL) /
        (max(spe$array_col) - min(spe$array_col))

    #   Find the appropriate spot size for this donor. This can vary because
    #   ggplot downscales a plot to fit desired output dimensions (in this
    #   case presumably a square region on a PDF), and stitched images can vary
    #   in aspect ratio. Also, lowres images always have a larger image
    #   dimension of 1200, no matter how many spots fit in either dimension.
    small_image_data <- imgData(spe)[
        imgData(spe)$image_id == image_id,
    ]

    #   The coefficient of 100 was determined empirically
    point_size <- point_size * 100 * INTER_SPOT_DIST_PX *
        small_image_data$scaleFactor / max(dim(small_image_data$data[[1]]))

    return(list(spe = spe, point_size = point_size))
}
