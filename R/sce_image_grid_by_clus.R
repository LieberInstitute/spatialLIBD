#' Title
#'
#' @param sce
#' @param clusters
#' @param pdf_file
#' @param colors
#' @param spatial
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

sce_image_grid_by_clus <-
    function(sce,
        clusters,
        pdf_file,
        colors = NULL,
        spatial = TRUE,
        ...) {
        if (is.null(colors)) {
            colors <- c('FALSE' = 'transparent', 'TRUE' = 'red')
        }
        clusters_uni <- sort(unique(clusters))
        pdf(pdf_file, height = 24, width = 36)
        lapply(clusters_uni, function(clus) {
            curr_clus <- factor(clusters == clus, levels = c('FALSE', 'TRUE'))
            plots <-
                sce_image_grid(
                    sce,
                    curr_clus,
                    sort_clust = FALSE,
                    colors = colors,
                    return_plots = TRUE,
                    spatial = spatial,
                    ... = paste(..., '- cluster', clus)
                )
            print(cowplot::plot_grid(plotlist = plots))
            return(clus)
        })
        dev.off()
        return(pdf_file)
    }
