#' Sample spatial gene visualization workhorse function for Xenium data with
#' centroid based spatailCoords
#'
#' This function visualizes the gene expression stored in `assays(spe)` or any
#' continuous variable stored in `colData(spe)` for one given sample at the
#' spot-level using (by default) the histology information on the background.
#' This is the function that does all the plotting behind [vis_clus()] when
#' `datatype = "Xenium"`. To visualize clusters (or any discrete variable)
#' use [vis_clus_c()].
#'
#' @inheritParams vis_gene_p
#' @inheritParams vis_clus_c
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
#'     if (!exists("spe_xenium")) spe_xenium <- fetch_data("spe_xenium_test")
#'
#'     ## Prepare the data for the plotting function
#'     spe_sub <- spe_xenium[, spe_xenium$sample_id == "Br1039"]
#'     df <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)
#'     df$COUNT <- df$detected_gex
#'
#'     ## Don't plot the histology information
#'     p <- vis_gene_c(
#'         spe = spe_sub,
#'         d = df,
#'         sampleid = "Br1039",
#'         title = "Br1039 detected_gex",
#'         point_size = 1
#'     )
#'     print(p)
#'
#'     ## Clean up
#'     rm(spe_sub)
#' }
vis_gene_c <-
    function(
        spe,
        d,
        sampleid = unique(spe$sample_id)[1],
        title,
        viridis = TRUE,
        alpha = NA,
        cont_colors = if (viridis) {
            viridisLite::viridis(21)
        } else {
            c("aquamarine4", "springgreen", "goldenrod", "red")
        },
        point_size = 2,
        na_color = "#CCCCCC40",
        legend_title = ""
    ) {
        ## Some variables
        y_centroid <- x_centroid <- key <- COUNT <- NULL
        # stopifnot(all(c("x_centroid", "y_centroid", "COUNT", "key") %in% colnames(d)))

        # ## Crop the image if needed
        # if (auto_crop) {
        #   frame_lims <-
        #     frame_limits(spe, sampleid = sampleid, image_id = image_id)
        #   img <-
        #     img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
        #   adjust <-
        #     list(x = frame_lims$x_min, y = frame_lims$y_min)
        # } else {
        adjust <- list(x = 0, y = 0)
        # }

        p <-
            ggplot(
                d,
                aes(
                    x = x_centroid,
                    y = y_centroid,
                    fill = COUNT,
                    color = COUNT,
                    key = key
                )
            )

        p <- p +
            geom_point(
                shape = 21,
                size = point_size,
                stroke = 0,
                colour = "transparent",
                alpha = alpha
            ) +
            coord_fixed(expand = FALSE)

        p <- p +
            scale_fill_gradientn(
                name = legend_title,
                colors = cont_colors,
                na.value = na_color
            ) +
            scale_color_gradientn(
                name = legend_title,
                colors = cont_colors,
                na.value = na_color
            )

        p <- p +
            xlab("") +
            ylab("") +
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
                legend.title = element_text(size = 10),
                legend.box.spacing = unit(0, "pt")
            )
        return(p)
    }
