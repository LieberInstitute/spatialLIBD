#' Sample spatial cluster visualization grid
#'
#' This function visualizes the clusters for a set of samples at the spot-level
#' using (by default) the histology information on the background. To visualize
#' gene-level (or any continuous variable) use [vis_grid_gene()].
#'
#' @inheritParams vis_clus
#' @param pdf_file A `character(1)` specifying the path for the resulting PDF.
#' @param sort_clust A `logical(1)` indicating whether you want to sort
#' the clusters by frequency using [sort_clusters()]. Defuault `FALSE`.
#' @param return_plots A `logical(1)` indicating whether to print the plots
#' to a PDF or to return the list of plots that you can then print using
#' [plot_grid][cowplot::plot_grid()].
#' @param height A `numeric(1)` passed to [pdf][grDevices::pdf()].
#' @param width A `numeric(1)` passed to [pdf][grDevices::pdf()].
#' @param sample_order A `character()` with the names of the samples to use
#' and their order.
#' @param guides A `character(1)` specifying which guides to print. Defaults to 
#' `all` which plots all guides. `last` prints a guide for only on the last 
#' sample. `none` prints no guides with the plots, but prints a guide on 
#' separate page.
#'
#' @return A list of [ggplot2][ggplot2::ggplot] objects.
#' @export
#' @importFrom grDevices pdf dev.off
#' @importFrom SummarizedExperiment colData<-
#' @importFrom cowplot plot_grid get_legend
#' @importFrom grid grid.newpage grid.draw
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
    sort_clust = FALSE,
    colors = NULL,
    return_plots = FALSE,
    spatial = TRUE,
    height = 24,
    width = 36,
    image_id = "lowres",
    alpha = NA,
    sample_order = unique(spe$sample_id),
    point_size = 2,
    auto_crop = TRUE,
    na_color = "#CCCCCC40",
    is_stitched = FALSE,
    guides = c("all", "last", "none"),
    title_suffix = NULL) {
      
      stopifnot(all(sample_order %in% unique(spe$sample_id)))
      ## check guides selection
      guides <- rlang::arg_match(guides)

        if (sort_clust) {
            colData(spe)[[clustervar]] <-
                sort_clusters(colData(spe)[[clustervar]])
        }
        plots <-
            lapply(sample_order, function(sampleid) {
                vis_clus(spe,
                    sampleid,
                    clustervar,
                    colors = colors,
                    spatial = spatial,
                    image_id = image_id,
                    alpha = alpha,
                    point_size = point_size,
                    auto_crop = auto_crop,
                    na_color = na_color,
                    is_stitched = is_stitched
                )
            })
        names(plots) <- sample_order
        
        if(!guides == "all"){
          ## get legend
          suppressWarnings(legend <- cowplot::get_legend(plots[[1]]))
          
          ## Set legend position to None on all plots
          noguide <- function(gp){
            gp + theme(legend.position = "None")
          }
          plots <- lapply(plots, noguide)
          
          if(guides == "last") {
            ## re-set legend in last plot
            plots[[length(plots)]] <- plots[[length(plots)]] + theme(legend.position = "right")
          } 
          
        }

        if (!return_plots) {
          if(guides %in% c("all", "last")){
            pdf(pdf_file, height = height, width = width)
            print(cowplot::plot_grid(plotlist = plots))
            dev.off()
            
          } else if(guides == "none"){
            ## print guide on next page
            pdf(pdf_file, height = height, width = width)
            print(cowplot::plot_grid(plotlist = plots))
            grid::grid.newpage()
            grid::grid.draw(legend)
            dev.off()
          }
            return(pdf_file)
        } else {
            return(plots)
        }
    }
