#' Identify the image limits
#'
#' This function is useful for automatically cropping the images. It finds
#' the edge points (min and max on both the X and Y axis) in pixels based
#' on a particular image. This function takes advantage of the known design
#' of Visium slides as documented at
#' <https://support.10xgenomics.com/spatial-gene-expression/software/pipelines/latest/output/spatial>
#' and
#' <https://kb.10xgenomics.com/hc/en-us/articles/360041426992>. That is,
#' that for a regular Visium slide, the array row has a range from 0 to 77,
#' the array col from 0 to 127, the capture area has a 6.5 mm edge length, the
#' the fiducial frame area has an edge of 8 mm, and spot centers are about 100
#' um from each other.
#'
#'
#' @inheritParams vis_gene_p
#' @param visium_grid A named list with the parameters known about the Visium
#' grid. This can change for Visium HD vs regular Visium for example.
#'
#' @return A named list with `y_min`, `y_max`, `x_min`, and `x_max` pixels from
#' the selected image that can be used for cropping the image.
#'
#' @author
#' Louise Huuki-Myers and Leonardo Collado-Torres
#'
#' @family Spatial cluster visualization functions
#'
#' @export
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Obtain the frame limits for one sample
#'     frame_limits(spe, sampleid = "151673", image_id = "lowres")
#' }
#'
frame_limits <- function(spe, sampleid, image_id, visium_grid = list(row_min = 0, row_max = 77, col_min = 0, col_max = 127, fiducial_vs_capture_edge = (8 - 6.5) * 1000 / 2 / 100)) {
    ## Subset the info we need for the particular sample
    d <- as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$sample_id == sampleid, ], optional = TRUE)

    ## Check we have all the pieces we need
    stopifnot(all(c("array_row", "array_col", "pxl_row_in_fullres", "pxl_col_in_fullres") %in% colnames(d)))
    stopifnot(all(c("row_min", "row_max", "col_min", "col_max", "fiducial_vs_capture_edge") %in% names(visium_grid)))

    ## Re-order by row then column
    d <- d[order(d$array_row, d$array_col), ]

    ## Compute the difference by array_col and pxl_col_in_fullres
    diffs_array <- abs(diff(d$array_col))
    diffs_pxl <- abs(diff(d$pxl_col_in_fullres))
    diffs_pxl_row <- abs(diff(d$pxl_row_in_fullres))
    if (median(diffs_pxl_row[diffs_array == 2] - diffs_pxl[diffs_array == 2]) > 0) {
        warning("frame_limits() doesn't seem to be working as expected. Maybe you have swapped pxl_col_in_fullres and pxl_row_in_fullres. Could be the case if your SpatialExperiment object was created prior to https://github.com/drighelli/SpatialExperiment/commit/6710fe8b0a7919191ecce989bb6831647385ef5f. Consider using: 'colnames(spatialCoords(spe)) <- rev(colnames(spatialCoords(spe)))'.", call. = FALSE)
    }
    pxl_100um <- median(diffs_pxl[diffs_array == 2])

    ## Locate edges on the array coordinates
    row_min <- min(d$array_row)
    row_max <- max(d$array_row)
    col_min <- min(d$array_col)
    col_max <- max(d$array_col)

    ## This is required to deal with some weird SPE objects, potentially due
    ## to them being created prior to
    ## https://github.com/drighelli/SpatialExperiment/commit/6710fe8b0a7919191ecce989bb6831647385ef5f
    ylims <- c(
        min(d$pxl_row_in_fullres[which(d$array_row == row_min)]),
        max(d$pxl_row_in_fullres[which(d$array_row == row_max)])
    )
    xlims <- c(
        min(d$pxl_col_in_fullres[which(d$array_col == col_min)]),
        max(d$pxl_col_in_fullres[which(d$array_col == col_max)])
    )

    ## Compute the frame limits in pixels in the full resolution image,
    ## then adjust for the pixels in the current image
    frame_lims <- lapply(list(
        y_min = min(ylims) + (visium_grid$row_min - row_min) * pxl_100um - visium_grid$fiducial_vs_capture_edge * pxl_100um,
        y_max = max(ylims) + (visium_grid$row_max - row_max) * pxl_100um + visium_grid$fiducial_vs_capture_edge * pxl_100um,
        x_min = min(xlims) + (visium_grid$col_min - col_min) * pxl_100um - visium_grid$fiducial_vs_capture_edge * pxl_100um,
        x_max = max(xlims) + (visium_grid$col_max - col_max) * pxl_100um + visium_grid$fiducial_vs_capture_edge * pxl_100um
    ), function(x) {
        ceiling(
            x * SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id)
        )
    })

    ## Obtain the dimensions of the image
    img <- SpatialExperiment::getImg(spe, sample_id = sampleid, image_id = image_id)


    ## Adjust frame limits to keep them within the expected limits
    frame_lims$y_min <- ifelse(frame_lims$y_min < 1, 1, frame_lims$y_min)
    frame_lims$y_max <- ifelse(frame_lims$y_max > nrow(img), nrow(img), frame_lims$y_max)
    frame_lims$x_min <- ifelse(frame_lims$x_min < 1, 1, frame_lims$x_min)
    frame_lims$x_max <- ifelse(frame_lims$x_max > ncol(img), ncol(img), frame_lims$x_max)

    ## Done!
    return(frame_lims)
}
