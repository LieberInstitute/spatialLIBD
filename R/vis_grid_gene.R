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
    function(spe,
    geneid = "SCGB2A2; ENSG00000110484",
    pdf_file,
    assayname = "logcounts",
    minCount = 0,
    return_plots = FALSE,
    spatial = TRUE,
    viridis = TRUE,
    height = 24,
    width = 36,
    ...) {
        stopifnot("gene_search" %in% colnames(rowData(spe)))
        plots <- lapply(unique(spe$sample_id), function(sampleid) {
            vis_gene(
                spe,
                sampleid,
                geneid,
                spatial,
                assayname,
                minCount,
                viridis,
                ...
            )
        })
        names(plots) <- unique(spe$sample_id)

        if (!return_plots) {
            pdf(pdf_file, height = height, width = width)
            print(cowplot::plot_grid(plotlist = plots))
            dev.off()
            return(pdf_file)
        }
        else {
            return(plots)
        }
    }
