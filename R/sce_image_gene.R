#' Sample spatial gene visualization
#'
#' This function visualizes the gene expression stored in `assays(sce)` or any
#' continuous variable stored in `colData(sce)` for one given sample at the
#' spot-level using (by default) the histology information on the background.
#' To visualize clusters (or any discrete variable) use [sce_image_clus()].
#'
#' @inheritParams sce_image_clus
#' @param geneid A `character(1)` specifying the gene ID stored in
#' `rowData(sce)$gene_search` or a continuous variable stored in `colData(sce)`
#' to visualize.
#' @param assayname The name of the `assays(sce)` to use for extracting the
#' gene expression data. Defaults to `logcounts`.
#' @param minCount A `numeric(1)` specifying the minimum gene expression (or
#' value in the continuous variable) to visualize. Values at or below this
#' threshold will be set to `NA`. Defaults to `0`.
#' @param viridis A `logical(1)` whether to use the color-blind friendly
#' palette from [viridis][viridisLite::viridis()] or the color palette used
#' in the paper that was chosen for contrast when visualizing the data on
#' top of the histology image. One issue is being able to differentiate low
#' values from NA ones due to the purple-ish histology information that is
#' dependent on cell density.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom SummarizedExperiment assays
#' @family Spatial gene visualization functions
#' @details This function subsets `sce` to the given sample and prepares the
#' data and title for [sce_image_gene_p()]. It also adds a caption to the plot.
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists('sce')) sce <- fetch_data('sce')
#'
#' ## Valid `geneid` values are those in
#' head(rowData(sce)$gene_search)
#' ## or continuous variables stored in colData(sce)
#'
#' ## Visualize a default gene on the non-viridis scale
#' sce_image_gene(
#'     sce = sce,
#'     sampleid = '151507',
#'     viridis = FALSE
#' )
#'
#' ## Visualize a continuous variable, in this case, the ratio of chrM
#' ## gene expression compared to the total expression at the spot-level
#' sce_image_gene(
#'     sce = sce,
#'     sampleid = '151507',
#'     geneid = 'expr_chrM_ratio'
#' )
#'

sce_image_gene <-
    function(sce,
        sampleid,
        geneid = "SCGB2A2; ENSG00000110484",
        spatial = TRUE,
        assayname = 'logcounts',
        minCount = 0,
        viridis = TRUE,
        ...) {
        sce_sub <- sce[, sce$sample_name == sampleid]
        d <- as.data.frame(colData(sce_sub))

        if (geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[assayname]][which(rowData(sce_sub)$gene_search == geneid), ]
        }
        d$COUNT[d$COUNT <= minCount] <- NA
        p <- sce_image_gene_p(
            sce = sce_sub,
            d = d,
            sampleid = sampleid,
            spatial = spatial,
            title = paste(sampleid,
                geneid,
                ...),
            viridis = viridis
        )
        p + labs(caption = paste(if (!geneid %in% colnames(colData(sce_sub)))
            assayname
            else
                NULL,
            'min >', minCount))
    }
