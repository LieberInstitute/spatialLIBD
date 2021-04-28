#' Check input spe
#'
#' This function checks that the `spe` object has the appropriate structure.
#' For more details please check the vignette documentation.
#'
#' @inheritParams sce_to_spe
#' @inheritParams vis_clus
#' @param variables A `character()` vector of variable names expected to
#' be present in `colData(spe)`.
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @importFrom SummarizedExperiment assayNames
#' @family Check input functions
#' @author
#' Brenda Pardo, Leonardo Collado-Torres
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Check the object
#'     check_spe(spe)
#' }
check_spe <- function(spe,
    variables = c(
        "GraphBased",
        "Layer",
        "cell_count",
        "sum_umi",
        "sum_gene",
        "expr_chrM",
        "expr_chrM_ratio"
    )) {
    ## Should be a SpatialExperiment object
    stopifnot(is(spe, "SpatialExperiment"))

    ## Images data stored under imgData(sce)
    stopifnot(all(c(
        "sample_id", "image_id", "data",
        "scaleFactor"
    ) %in% colnames(imgData(spe))))

    ## Check that the images have been loaded
    stopifnot(all(vapply(imgData(spe)$data, is, logical(1), "SpatialImage")))

    ## Check gene data
    stopifnot(all(
        c("gene_id", "gene_name", "gene_search") %in%
            colnames(rowData(spe))
    ))
    stopifnot(identical(
        paste0(rowData(spe)$gene_name, "; ", rowData(spe)$gene_id),
        rowData(spe)$gene_search
    ))

    ## Rownames are Ensembl ids
    stopifnot(identical(rownames(spe), rowData(spe)$gene_id))

    ## Information about spot coordinates
    stopifnot(all(c(
        "in_tissue", "array_row", "array_col",
        "pxl_row_in_fullres", "pxl_col_in_fullres"
    ) %in%
        colnames(spatialData(spe))))

    ## colData(spe) includes information about the samples .
    ## The sample names stored under spe$sample_id
    stopifnot(all(
        c(
            "Barcode",
            "sample_id",
            "key",
            "Layer",
            variables
        ) %in% colnames(colData(spe))
    ))

    ## A unique spot-level ID (such as barcode) stored under spe$key
    ## The 'key' column is necessary for the plotly code to work.
    stopifnot(length(unique(spe$key)) == ncol(spe))
    stopifnot(identical(spe$key, paste0(spe$sample_id, "_", colData(spe)$Barcode)))

    ## None of the values of rowData(spe)$gene_search should be re-used in
    ## colData(spe)
    stopifnot(!all(rowData(spe)$gene_search %in% colnames(colData(spe))))

    ## No column named COUNT as that's used by vis_gene_p() and related
    ## functions.
    stopifnot(!"COUNT" %in% colnames(colData(spe)))

    ## The counts and logcounts assays
    stopifnot(length(assayNames(spe)) >= 1)
    # checar que assaynames sea mayorigual a uno lenght

    ## Done!
    return(spe)
}
