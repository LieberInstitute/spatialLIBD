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
#' to visualize. If `rowData(spe)$gene_search` is missing, then `rownames(spe)`
#' is used to search for the gene ID.
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
#' @param cont_colors A `character()` vector of colors that supersedes the
#' `viridis` argument.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom SummarizedExperiment assays
#' @importFrom SpatialExperiment spatialCoords
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
#'     ## or rownames(spe)
#'
#'     ## Visualize a default gene on the non-viridis scale
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         viridis = FALSE
#'     )
#'
#'     ## Use a custom set of colors in the reverse order than usual
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         cont_colors = rev(viridisLite::viridis(21, option = "magma"))
#'     )
#'
#'     ## Visualize a continuous variable, in this case, the ratio of chrM
#'     ## gene expression compared to the total expression at the spot-level
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = "expr_chrM_ratio"
#'     )
#'
#'     ## Visualize a gene using the rownames(spe)
#'     vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = rownames(spe)[which(rowData(spe)$gene_name == "MOBP")]
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
    image_id = "lowres",
    alpha = 1,
    cont_colors = if (viridis) viridisLite::viridis(21) else c("aquamarine4", "springgreen", "goldenrod", "red"),
    point_size = 2,
    ...) {
        spe_sub <- spe[, spe$sample_id == sampleid]
        d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)

        if (geneid %in% colnames(colData(spe_sub))) {
            d$COUNT <- colData(spe_sub)[[geneid]]
        } else if (geneid %in% rowData(spe_sub)$gene_search) {
            d$COUNT <-
                assays(spe_sub)[[assayname]][which(rowData(spe_sub)$gene_search == geneid), ]
        } else if (geneid %in% rownames(spe_sub)) {
            d$COUNT <- assays(spe_sub)[[assayname]][which(rownames(spe_sub) == geneid), ]
        } else {
            stop("Could not find the 'geneid' ", geneid, call. = FALSE)
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
            viridis = viridis,
            image_id = image_id,
            alpha = alpha,
            cont_colors = cont_colors,
            point_size = point_size
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
