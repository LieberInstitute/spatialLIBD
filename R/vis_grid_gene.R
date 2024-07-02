#' Sample spatial gene visualization grid
#'
#' This function visualizes the gene expression stored in `assays(spe)` or any
#' continuous variable stored in `colData(spe)` for a set of samples at the
#' spot-level using (by default) the histology information on the background.
#' To visualize clusters (or any discrete variable) use [vis_grid_clus()].
#'
#' @inheritParams vis_gene
#' @inheritParams vis_grid_clus
#'
#' @return A list of [ggplot2][ggplot2::ggplot] objects.
#' @export
#' @importFrom grDevices pdf dev.off
#' @family Spatial gene visualization functions
#' @details This function prepares the data and then loops through
#' [vis_gene()] for computing the list of [ggplot2][ggplot2::ggplot]
#' objects.
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Subset to two samples of interest and obtain the plot list
#'     p_list <-
#'         vis_grid_gene(
#'             spe[, spe$sample_id %in% c("151673", "151674")],
#'             spatial = FALSE,
#'             return_plots = TRUE
#'         )
#'
#'     ## Visualize the spatial adjacent replicates for position = 0 micro meters
#'     ## for subject 3
#'     cowplot::plot_grid(plotlist = p_list, ncol = 2)
#' }
vis_grid_gene <-
    function(
        spe,
        geneid = rowData(spe)$gene_search[1],
        pdf_file,
        assayname = "logcounts",
        minCount = 0,
        return_plots = FALSE,
        spatial = TRUE,
        viridis = TRUE,
        height = 24,
        width = 36,
        image_id = "lowres",
        alpha = NA,
        cont_colors = if (viridis) viridisLite::viridis(21) else c("aquamarine4", "springgreen", "goldenrod", "red"),
        sample_order = unique(spe$sample_id),
        point_size = 2,
        auto_crop = TRUE,
        na_color = "#CCCCCC40",
        is_stitched = FALSE,
        ...) {
        stopifnot(all(sample_order %in% unique(spe$sample_id)))

        plots <- lapply(sample_order, function(sampleid) {
            vis_gene(
                spe,
                sampleid,
                geneid,
                spatial,
                assayname,
                minCount,
                viridis,
                image_id = image_id,
                alpha = alpha,
                cont_colors = cont_colors,
                point_size = point_size,
                auto_crop = auto_crop,
                na_color = na_color,
                is_stitched = is_stitched,
                ...
            )
        })
        names(plots) <- sample_order

        if (!return_plots) {
            pdf(pdf_file, height = height, width = width)
            print(cowplot::plot_grid(plotlist = plots))
            dev.off()
            return(pdf_file)
        } else {
            return(plots)
        }
    }
