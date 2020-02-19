#' Title
#'
#' @param sce
#' @param geneid
#' @param pdf_file
#' @param assayname
#' @param minCount
#' @param return_plots
#' @param spatial
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

sce_image_grid_gene <-
    function(sce,
        geneid = "SCGB2A2; ENSG00000110484",
        pdf_file,
        assayname = 'logcounts',
        minCount = 0,
        return_plots = FALSE,
        spatial = TRUE,
        ...) {
        plots <- lapply(unique(sce$sample_name), function(sampleid) {
            sce_image_clus_gene(sce, sampleid, geneid, spatial, assayname, minCount, ...)
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
