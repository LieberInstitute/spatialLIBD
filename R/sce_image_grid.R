#' Title
#'
#' @param sce
#' @param clusters
#' @param pdf_file
#' @param sort_clust
#' @param colors
#' @param return_plots
#' @param spatial
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

sce_image_grid <-
    function(sce,
        clusters,
        pdf_file,
        sort_clust = TRUE,
        colors = NULL,
        return_plots = FALSE,
        spatial = TRUE,
        ...) {
        colors <- get_colors(colors, clusters)
        if (sort_clust) colData(sce)[[clusters]] <- sort_clusters(colData(sce)[[clusters]])
        plots <-
            lapply(unique(sce$sample_name), function(sampleid) {
                sce_image_clus(sce,
                    sampleid,
                    clusters,
                    colors = colors,
                    spatial = spatial,
                    ...)
            })
        names(plots) <- unique(sce$sample_name)
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
