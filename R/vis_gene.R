#' Sample spatial gene visualization
#'
#' This function visualizes the gene expression stored in `assays(spe)` or any
#' continuous variable stored in `colData(spe)` for one given sample at the
#' spot-level using (by default) the histology information on the background.
#' To visualize clusters (or any discrete variable) use [vis_clus()].
#'
#' @inheritParams vis_clus
#' @param geneid A `character(1)` specifying the gene ID stored in
#' `rowData(spe)$gene_search` or a continuous variable stored in `colData(spe)`
#' to visualize.
#' @param assayname The name of the `assays(spe)` to use for extracting the
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
#' @importFrom SpatialExperiment spatialData
#' @importFrom SummarizedExperiment assays
#' @family Spatial gene visualization functions
#' @details This function subsets `spe` to the given sample and prepares the
#' data and title for [vis_gene_p()]. It also adds a caption to the plot.
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Valid `geneid` values are those in
#'     head(rowData(spe)$gene_search)
#'     ## or continuous variables stored in colData(spe)
#'
#'     ## Visualize a default gene on the non-viridis scale
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         viridis = FALSE
#'     )
#'
#'     ## Visualize a continuous variable, in this case, the ratio of chrM
#'     ## gene expression compared to the total expression at the spot-level
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = "expr_chrM_ratio"
#'     )
#' }
vis_gene <-
    function(spe,
    sampleid,
    geneid = "SCGB2A2; ENSG00000110484",
    spatial = TRUE,
    assayname = "logcounts",
    minCount = 0,
    viridis = TRUE,
    ...) {
        spe_sub <- spe[, spe$sample_id == sampleid]
        d <- as.data.frame(SpatialExperiment::spatialData(spe_sub, cd_bind = TRUE))
        stopifnot("gene_search" %in% colnames(rowData(spe)))

        if (geneid %in% colnames(colData(spe_sub))) {
            d$COUNT <- colData(spe_sub)[[geneid]]
        } else {
            d$COUNT <-
                assays(spe_sub)[[assayname]][which(rowData(spe_sub)$gene_search == geneid), ]
        }
        d$COUNT[d$COUNT <= minCount] <- NA
        p <- vis_gene_p(
            spe = spe_sub,
            d = d,
            sampleid = sampleid,
            spatial = spatial,
            title = paste(
                sampleid,
                geneid,
                ...
            ),
            viridis = viridis
        )
        p + labs(caption = paste(
            if (!geneid %in% colnames(colData(spe_sub))) {
                assayname
            } else {
                NULL
            },
            "min >", minCount
        ))
    }
