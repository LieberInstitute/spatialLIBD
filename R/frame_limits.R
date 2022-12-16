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
#'     frame_limits(spe, sampleid = "151673")
#' }
#'
frame_limits <-
    function(spe,
    sampleid,
    image_id = "lowres",
    visium_grid = list(
        row_min = 0,
        row_max = 77,
        col_min = 0,
        col_max = 127,
        fiducial_vs_capture_edge = (8 - 6.5) * 1000 / 2 / 100
    )) {
        ## Subset the info we need for the particular sample
        d <-
            as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$sample_id == sampleid, ],
                optional = TRUE
            )

        ## Check we have all the pieces we need
        stopifnot(all(
            c(
                "array_row",
                "array_col",
                "pxl_row_in_fullres",
                "pxl_col_in_fullres"
            ) %in% colnames(d)
        ))
        stopifnot(all(
            c(
                "row_min",
                "row_max",
                "col_min",
                "col_max",
                "fiducial_vs_capture_edge"
            ) %in% names(visium_grid)
        ))

        ## Locate edges on the array coordinates
        row_min <- min(d$array_row)
        row_max <- max(d$array_row)
        col_min <- min(d$array_col)
        col_max <- max(d$array_col)

        ## Check that they are within the Visium grid parameters
        stopifnot(row_min >= visium_grid$row_min)
        stopifnot(row_max <= visium_grid$row_max)
        stopifnot(col_min >= visium_grid$col_min)
        stopifnot(col_max <= visium_grid$col_max)

        ## Compute the distance to row/col edges
        d$array_row_edge <-
            sapply(d$array_row, function(x) {
                min(abs(c(
                    visium_grid$row_min, visium_grid$row_max
                ) - x))
            })
        d$array_col_edge <-
            sapply(d$array_col, function(x) {
                min(abs(c(
                    visium_grid$col_min, visium_grid$col_max
                ) - x))
            })
        d$array_min_edge <- pmin(d$array_row_edge, d$array_col_edge)

        ## Compute the distance in pixels for 100 um
        pxl_100um_compute <- function(array_columns) {
            d <- d[order(d[[array_columns[[1]]]], d[[array_columns[[2]]]]), ]

            diffs_array <- abs(diff(d[[array_columns[[2]]]]))

            diffs_pxl_col <- abs(diff(d$pxl_col_in_fullres))
            diffs_pxl_row <- abs(diff(d$pxl_row_in_fullres))
            c(median(diffs_pxl_col[diffs_array == 2]), median(diffs_pxl_row[diffs_array == 2]))
        }
        pxl_100um_compute(c("array_col", "array_row"))
        pxl_100um_compute(c("array_row", "array_col"))

        # [1]   2 240
        # [1] 138   1

        # [1] 482   1
        # [1]   0 277


        pxl_100um_col_then_row <-
            max(pxl_100um_compute(c("array_col", "array_row")))
        pxl_100um_row_then_col <-
            max(pxl_100um_compute(c("array_row", "array_col")))

        print(c(pxl_100um_row_then_col, pxl_100um_col_then_row))


        ## It should normally be pxl_100um_row_then_col but just in case
        pxl_100um <- min(pxl_100um_row_then_col, pxl_100um_col_then_row)
        pxl_100um

        options(width = 160)
        do.call(rbind, list(
            d[d$pxl_row_in_fullres == min(d$pxl_row_in_fullres), c(
                "array_col",
                "array_row",
                "pxl_col_in_fullres",
                "pxl_row_in_fullres",
                "array_row_edge",
                "array_col_edge",
                "array_min_edge"
            )],
            d[d$pxl_row_in_fullres == max(d$pxl_row_in_fullres), c(
                "array_col",
                "array_row",
                "pxl_col_in_fullres",
                "pxl_row_in_fullres",
                "array_row_edge",
                "array_col_edge",
                "array_min_edge"
            )],
            d[d$pxl_col_in_fullres == min(d$pxl_col_in_fullres), c(
                "array_col",
                "array_row",
                "pxl_col_in_fullres",
                "pxl_row_in_fullres",
                "array_row_edge",
                "array_col_edge",
                "array_min_edge"
            )],
            d[d$pxl_col_in_fullres == max(d$pxl_col_in_fullres), c(
                "array_col",
                "array_row",
                "pxl_col_in_fullres",
                "pxl_row_in_fullres",
                "array_row_edge",
                "array_col_edge",
                "array_min_edge"
            )]
        ))
        #                    array_col array_row pxl_col_in_fullres pxl_row_in_fullres array_row_edge array_col_edge array_min_edge
        # GAGCGCTATGTCAGGC-1        20         0               4188               2437              0             20              0
        # CCCGGCACGTGTCAGG-1       104        76               9906              11584              1             23              1
        # GCAAACGTAAGCGACC-1         5        69               3096              10696              8              5              5
        # GGTCATTGTAGTCATA-1       120        14              11062               4164             14              7              7
        #
        # xmin: min pxl_row, min array_row
        # xmax: max pxl_row, max array_row
        # ymin: min pxl_col, min array_col
        # ymax: max pxl_col, max array_col

        #                    array_col array_row pxl_col_in_fullres pxl_row_in_fullres array_row_edge array_col_edge array_min_edge
        # TGTTCTCATACTATAG-1       127        71              20140               6824              6              0              0
        # TCGCACTAACGTTTGT-1         0        14               6432              24421             14              0              0
        # GCGAGGGACTGCTAGA-1         7         1               3299              23455              1              7              1
        # GCCCAGCGACACAAAG-1         1        77              21606              24265              0              1              0
        # AAGCTATGGATTGACC-1         3        77              21606              23988              0              3              0
        #
        # xmin: min pxl_row, max array_col
        # xmax: max pxl_row, min array_col
        # ymin: min pxl_col, min array_row
        # ymax: max pxl_col, max array_row


        ## Find the full res pixel limits
        xlims <- range(d$pxl_col_in_fullres)
        ylims <- range(d$pxl_row_in_fullres)
        c(xlims, ylims)

        ## Find the array edge limit distance
        xlims_edge <- sapply(xlims, function(x) {
            min(d$array_min_edge[d$pxl_col_in_fullres == x])
        })
        ylims_edge <- sapply(ylims, function(x) {
            min(d$array_min_edge[d$pxl_row_in_fullres == x])
        })
        c(xlims_edge, ylims_edge)

        ## Obtain the dimensions of the image
        img <-
            SpatialExperiment::getImg(spe, sample_id = sampleid, image_id = image_id)

        ## Compute frame limits in full res pixels
        frame_lims_pxl <- list(
            y_min = ylims[1] - ylims_edge[1] * pxl_100um - visium_grid$fiducial_vs_capture_edge * pxl_100um,
            # * nrow(img) / ncol(img),
            y_max = ylims[2] + ylims_edge[2] * pxl_100um + visium_grid$fiducial_vs_capture_edge * pxl_100um,
            # * nrow(img) / ncol(img),
            x_min = xlims[1] - xlims_edge[1] * pxl_100um - visium_grid$fiducial_vs_capture_edge * pxl_100um,
            x_max = xlims[2] + xlims_edge[2] * pxl_100um + visium_grid$fiducial_vs_capture_edge * pxl_100um
        )
        frame_lims_pxl

        ## Adjust for the pixels in the current image
        frame_lims <- lapply(frame_lims_pxl, function(x) {
            ceiling(
                x * SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id)
            )
        })
        frame_lims

        ## Adjust frame limits to keep them within the expected limits
        frame_lims$y_min <-
            ifelse(frame_lims$y_min < 1, 1, frame_lims$y_min)
        frame_lims$y_max <-
            ifelse(frame_lims$y_max > nrow(img), nrpw(img), frame_lims$y_max)
        frame_lims$x_min <-
            ifelse(frame_lims$x_min < 1, 1, frame_lims$x_min)
        frame_lims$x_max <-
            ifelse(frame_lims$x_max > ncol(img), ncol(img), frame_lims$x_max)
        frame_lims

        ## Done!
        return(frame_lims)
    }
