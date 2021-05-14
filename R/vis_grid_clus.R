#' Sample spatial cluster visualization grid
#'
#' This function visualizes the clusters for a set of samples at the spot-level
#' using (by default) the histology information on the background. To visualize
#' gene-level (or any continuous variable) use [vis_grid_gene()].
#'
#' @inheritParams vis_clus
#' @param pdf_file A `character(1)` specifying the path for the resulting PDF.
#' @param sort_clust A `logical(1)` indicating whether you want to sort
#' the clusters by frequency using [sort_clusters()].
#' @param return_plots A `logical(1)` indicating whether to print the plots
#' to a PDF or to return the list of plots that you can then print using
#' [plot_grid][cowplot::plot_grid()].
#' @param height A `numeric(1)` passed to [pdf][grDevices::pdf()].
#' @param width A `numeric(1)` passed to [pdf][grDevices::pdf()].
#'
#' @return A list of [ggplot2][ggplot2::ggplot] objects.
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom SummarizedExperiment colData<-
#' @family Spatial cluster visualization functions
#' @details This function prepares the data and then loops through
#' [vis_clus()] for computing the list of [ggplot2][ggplot2::ggplot]
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
#'         vis_grid_clus(
#'             spe[, spe$sample_id %in% c("151673", "151674")],
#'             "layer_guess_reordered",
#'             spatial = FALSE,
#'             return_plots = TRUE,
#'             sort_clust = FALSE,
#'             colors = libd_layer_colors
#'         )
#'
#'     ## Visualize the spatial adjacent replicates for position = 0 micro meters
#'     ## for subject 3
#'     cowplot::plot_grid(plotlist = p_list, ncol = 2)
#' }
vis_grid_clus <-
    function(spe,
    clustervar,
    pdf_file,
    sort_clust = TRUE,
    colors = NULL,
    return_plots = FALSE,
    spatial = TRUE,
    height = 24,
    width = 36,
    ...) {
        if (sort_clust) {
            colData(spe)[[clustervar]] <-
                sort_clusters(colData(spe)[[clustervar]])
        }
        plots <-
            lapply(unique(spe$sample_id), function(sampleid) {
                vis_clus(spe,
                    sampleid,
                    clustervar,
                    colors = colors,
                    spatial = spatial,
                    ...
                )
            })
        names(plots) <- unique(spe$sample_id)


        if (!return_plots) {
            pdf(pdf_file, height = 24, width = 36)
            print(cowplot::plot_grid(plotlist = plots))
            dev.off()
            return(pdf_file)
        }
        else {
            return(plots)
        }
    }
