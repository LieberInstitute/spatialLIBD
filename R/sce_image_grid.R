#' Sample spatial cluster visualization grid
#'
#' This function visualizes the clusters for a set of samples at the spot-level
#' using (by default) the histology information on the background. To visualize
#' gene-level (or any continuous variable) use [sce_image_grid_gene()].
#'
#' @inheritParams sce_image_clus
#' @param pdf_file A `character(1)` specifying the path for the resulting PDF.
#' @param sort_clust A `logical(1)` indicating whether you want to sort
#' the clusters by frequency using [sort_clusters()].
#' @param return_plots A `logical(1)` indicating whether to print the plots
#' to a PDF or to return the list of plots that you can then print using
#' [plot_grid][cowplot::plot_grid()].
#'
#' @return A list of [ggplot2][ggplot2::ggplot] objects.
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom SummarizedExperiment colData<-
#' @family Spatial cluster visualization functions
#' @details This function prepares the data and then loops through
#' [sce_image_clus()] for computing the list of [ggplot2][ggplot2::ggplot]
#' objects.
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("sce")) sce <- fetch_data("sce")
#'
#'     ## Subset to two samples of interest
#'     sce_sub <- sce[, sce$sample_name %in% c("151673", "151674")]
#'
#'     ## Obtain the plot list
#'     p_list <-
#'         sce_image_grid(
#'             sce_sub,
#'             "layer_guess_reordered",
#'             spatial = FALSE,
#'             return_plots = TRUE,
#'             sort_clust = FALSE,
#'             colors = libd_layer_colors
#'         )
#'         
#'      ## Or you can do this with a VisiumEsperiment object
#'      ve_sub <- sce_to_ve(sce_sub)
#'      p_list <-
#'         sce_image_grid(
#'             ve_sub,
#'             "layer_guess_reordered",
#'             spatial = FALSE,
#'             return_plots = TRUE,
#'             sort_clust = FALSE,
#'             colors = libd_layer_colors
#'         )
#'      
#'     ## Clean up
#'     rm(sce_sub)
#'
#'     ## Visualize the spatial adjacent replicates for position = 0 micro meters
#'     ## for subject 3
#'     cowplot::plot_grid(plotlist = p_list, ncol = 2)
#' }
sce_image_grid <-
    function(sce,
    clustervar,
    pdf_file,
    sort_clust = TRUE,
    colors = NULL,
    return_plots = FALSE,
    spatial = TRUE,
    ...) {
        

        if (sort_clust) {
            colData(sce)[[clustervar]] <-
                sort_clusters(colData(sce)[[clustervar]])
          }
        plots <-
            lapply(if (is(sce, "VisiumExperiment")) {unique(SpatialExperiment::spatialCoords(ve_sub)$sample_name)} else{unique(sce$sample_name)}, function(sampleid) {
                sce_image_clus(sce,
                    sampleid,
                    clustervar,
                    colors = colors,
                    spatial = spatial,
                    ...
                )
            })
        
        if (is(sce, "VisiumExperiment")) {    
            names(plots) <- unique(SpatialExperiment::spatialCoords(ve_sub)$sample_name)
        }else{
            names(plots) <- unique(sce$sample_name)
        }
        
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
