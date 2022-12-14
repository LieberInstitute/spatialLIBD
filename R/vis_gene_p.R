#' Sample spatial gene visualization workhorse function
#'
#' This function visualizes the gene expression stored in `assays(spe)` or any
#' continuous variable stored in `colData(spe)` for one given sample at the
#' spot-level using (by default) the histology information on the background.
#' This is the function that does all the plotting behind [vis_gene()].
#' To visualize clusters (or any discrete variable) use [vis_clus_p()].
#'
#' @param d A data.frame with the sample-level information. This is typically
#' obtained using `cbind(colData(spe), spatialCoords(spe))`.
#' The data.frame has to contain
#' a column with the continuous variable data to plot stored under `d$COUNT`.
#' @inheritParams vis_clus_p
#' @inheritParams vis_gene
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom tibble tibble
#' @importFrom SpatialExperiment imgData scaleFactors
#' @importFrom S4Vectors metadata
#' @importFrom grid rasterGrob unit
#' @family Spatial gene visualization functions
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Prepare the data for the plotting function
#'     spe_sub <- spe[, spe$sample_id == "151673"]
#'     df <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)
#'     df$COUNT <- df$expr_chrM_ratio
#'
#'     ## Use the manual color palette by Lukas M Weber
#'     ## Don't plot the histology information
#'     vis_gene_p(
#'         spe = spe_sub,
#'         d = df,
#'         sampleid = "151673",
#'         title = "151673 chrM expr ratio",
#'         spatial = FALSE
#'     )
#'
#'     ## Clean up
#'     rm(spe_sub)
#' }
vis_gene_p <-
    function(spe,
    d,
    sampleid,
    spatial,
    title,
    viridis = TRUE,
    image_id = "lowres",
    alpha = 1,
    cont_colors = if (viridis) viridisLite::viridis(21) else c("aquamarine4", "springgreen", "goldenrod", "red"),
    point_size = 2,
    auto_crop = TRUE) {

        ## Some variables
        pxl_row_in_fullres <- pxl_col_in_fullres <- key <- COUNT <- NULL
        # stopifnot(all(c("pxl_col_in_fullres", "pxl_row_in_fullres", "COUNT", "key") %in% colnames(d)))
        img <- SpatialExperiment::imgRaster(spe, sample_id = sampleid, image_id = image_id)

        ## Crop the image if needed
        if(auto_crop) {
            frame_lims <- frame_limits(spe, sampleid = sampleid, image_id = image_id)
            img <- img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
            adjust <- list(x = frame_lims$x_min, y = frame_lims$y_min)
        } else {
            adjust <- list(x = 0, y = 0)
        }

        p <-
            ggplot(
                d,
                aes(
                    x = pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id) - adjust$x,
                    y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id) - adjust$y,
                    fill = COUNT,
                    color = COUNT,
                    key = key
                )
            )

        if (spatial) {
            grob <- grid::rasterGrob(img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"))
            p <-
                p + geom_spatial(
                    data = tibble::tibble(grob = list(grob)),
                    aes(grob = grob),
                    x = 0.5,
                    y = 0.5
                )
        }

        ## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_marker_genes_plots.R
        # add.alpha('black', 0.175)
        # black
        # "#0000002D"

        p <- p +
            geom_point(
                shape = 21,
                size = point_size,
                stroke = 0,
                alpha = alpha
            ) +
            coord_cartesian(expand = FALSE)

        p <- p + scale_fill_gradientn(
            colors = cont_colors,
            na.value = c("black" = "#0000002D")
        ) +
            scale_color_gradientn(
                colors = cont_colors,
                na.value = c("black" = "#0000002D")
            )

        p <- p +
            xlim(0, ncol(img)) +
            ylim(nrow(img), 0) +
            xlab("") + ylab("") +
            labs(fill = NULL, color = NULL) +
            ggtitle(title) +
            theme_set(theme_bw(base_size = 20)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = c(0.95, 0.10)
            )
        return(p)
    }
