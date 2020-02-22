#' Sample spatial gene visualization grid
#'
#' This function visualizes the gene expression stored in `assays(sce)` or any
#' continuous variable stored in `colData(sce)` for a set of samples at the
#' spot-level using (by default) the histology information on the background.
#' To visualize clusters (or any discrete variable) use [sce_image_grid()].
#'
#' @inheritParams sce_image_gene
#' @inheritParams sce_image_grid
#'
#' @return A list of [ggplot2][ggplot2::ggplot] objects.
#' @export
#' @family Spatial gene visualization functions
#' @details This function prepares the data and then loops through
#' [sce_image_gene()] for computing the list of [ggplot2][ggplot2::ggplot]
#' objects.
#'
#' @examples
#'
#' ## Obtain the necessary data
#' ori_sce <- fetch_data('sce')
#'
#' ## Subset to two samples of interest
#' sce_sub <- ori_sce[, ori_sce$sample_name %in% c('151673', '151674')]
#'
#' ## Obtain the plot list
#' p_list <-
#'     sce_image_grid_gene(
#'         sce_sub,
#'         spatial = FALSE,
#'         return_plots = TRUE
#'     )
#'
#' ## Visualize the spatial adjacent replicates for position = 0 micro meters
#' ## for subject 3
#' cowplot::plot_grid(plotlist = p_list, ncol = 2)

sce_image_grid_gene <-
    function(sce,
        geneid = "SCGB2A2; ENSG00000110484",
        pdf_file,
        assayname = 'logcounts',
        minCount = 0,
        return_plots = FALSE,
        spatial = TRUE,
        viridis = TRUE,
        ...) {
        plots <- lapply(unique(sce$sample_name), function(sampleid) {
            sce_image_gene(sce,
                sampleid,
                geneid,
                spatial,
                assayname,
                minCount,
                viridis,
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
