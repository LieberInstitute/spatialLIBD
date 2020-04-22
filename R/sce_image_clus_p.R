#' Sample spatial cluster visualization workhorse function
#'
#' This function visualizes the clusters for one given sample at the spot-level
#' using (by default) the histology information on the background. This is the
#' function that does all the plotting behind [sce_image_clus()]. To visualize
#' gene-level (or any continuous variable) use [sce_image_gene_p()].
#'
#' @inheritParams sce_image_clus
#' @param d A data.frame with the sample-level information. This is typically
#' obtained using `as.data.frame(colData(sce))`.
#' @param title The title for the plot.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom S4Vectors metadata
#' @family Spatial cluster visualization functions
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("sce")) sce <- fetch_data("sce")
#'     sce_sub <- sce[, sce$sample_name == "151673"]
#'
#'     ## Use the manual color palette by Lukas M Weber
#'     ## Don't plot the histology information
#'     sce_image_clus_p(
#'         sce = sce_sub,
#'         d = as.data.frame(colData(sce_sub)),
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         title = "151673 LIBD Layers",
#'         spatial = FALSE
#'     )
#'
#'     ## Clean up
#'     rm(sce_sub)
#' }
sce_image_clus_p <-
    function(sce,
    d,
    clustervar,
    sampleid,
    colors,
    spatial,
    title) {

        ## Some variables
        imagecol <- imagerow <- key <- NULL

        if (clustervar %in% c(
            "layer_guess",
            "layer_guess_reordered",
            "layer_guess_reordered_short",
            "spatialLIBD"
        )) {
            title <- gsub(clustervar, "LIBD Layers", title)
        }

        p <- ggplot(
            d,
            aes(
                x = imagecol,
                y = imagerow,
                fill = factor(!!sym(clustervar)),
                key = key
            )
        )
        if (spatial) {
            p <-
                p + geom_spatial(
                    data = subset(metadata(sce)$image, sample == sampleid),
                    aes(grob = grob),
                    x = 0.5,
                    y = 0.5
                )
        }
        p <- p +
            geom_point(
                shape = 21,
                size = 1.25,
                stroke = 0.25
            ) +
            coord_cartesian(expand = FALSE) +
            scale_fill_manual(values = colors) +
            xlim(0, max(sce$width)) +
            ylim(max(sce$height), 0) +
            xlab("") + ylab("") +
            labs(fill = NULL) +
            guides(fill = guide_legend(override.aes = list(size = 3))) +
            ggtitle(title) +
            theme_set(theme_bw(base_size = 20)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )
        return(p)
    }
