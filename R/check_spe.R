#' Check input spe
#'
#' This function checks that the `spe` object has the appropriate structure.
#' For more details please check the vignette documentation.
#'
#'
#' @inheritParams sce_to_spe
#' @param variables A `character()` vector of variable names expected to
#' be present in `colData(spe)`.
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @importFrom SummarizedExperiment assayNames
#' @family Check input functions
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) sce <- fetch_data("spe")
#'
#'     ## Check the object
#'     check_sce(spe)
#' }
check_spe <- function(spe,
    variables = c(
        "GraphBased",
        "Layer",
        "Maynard",
        "Martinowich",
        paste0("SNN_k50_k", 4:28),
        "layer_guess_reordered_short",
        "cell_count",
        "sum_umi",
        "sum_gene",
        "expr_chrM",
        "expr_chrM_ratio",
        "SpatialDE_PCA",
        "SpatialDE_pool_PCA",
        "HVG_PCA",
        "pseudobulk_PCA",
        "markers_PCA",
        "SpatialDE_UMAP",
        "SpatialDE_pool_UMAP",
        "HVG_UMAP",
        "pseudobulk_UMAP",
        "markers_UMAP",
        "SpatialDE_PCA_spatial",
        "SpatialDE_pool_PCA_spatial",
        "HVG_PCA_spatial",
        "pseudobulk_PCA_spatial",
        "markers_PCA_spatial",
        "SpatialDE_UMAP_spatial",
        "SpatialDE_pool_UMAP_spatial",
        "HVG_UMAP_spatial",
        "pseudobulk_UMAP_spatial",
        "markers_UMAP_spatial"
    )) {
    ## Should be a SpatialExperiment object
    stopifnot(is(spe, "SpatialExperiment"))

    ## Images data stored under imgData(sce)
    stopifnot(all(c("sample_id", "image_id", "data",
                    "scaleFactor") %in% colnames(imgData(spe))))

    ## Check gene data
    stopifnot(all(
        c("source", "type", "gene_id", "gene_version", "gene_name", 
          "gene_source", "gene_biotype", "gene_search",  "is_top_hvg" ) %in% 
          colnames(rowData(spe))
    ))
    stopifnot(identical(
        paste0(rowData(spe)$gene_name, "; ", rowData(spe)$gene_id),
        rowData(spe)$gene_search
    ))

    ## Rownames are Ensembl ids
    stopifnot(identical(rownames(spe), rowData(spe)$gene_id))

    ## Information about spot coordinates
    stopifnot(all(c("barcode", "in_tissue", "array_row", "array_col",
                    "pxl_row_in_fullres", "pxl_col_in_fullres") %in% 
                    colnames(spatialData(spe))))
    
    ## colData(spe) includes information about the samples .
    ## The sample names stored under spe$sample_id
    stopifnot(all(c("sample_id", "subject", "position", "replicate", 
                    "subject_position", "key") %in% 
                    colnames(colData(spe))))

    ## Some cluster variables, though you could potentially edit
    ## spatialLIBD::app_ui()
    ## or this could be made into a run_app() argument.

    ## A unique spot-level ID (such as barcode) stored under sce$key
    ## The 'key' column is necessary for the plotly code to work.
    stopifnot(length(unique(spe$key)) == ncol(spe))
    identical(spe$key, paste0(spe$sample_id, '_', spatialData(spe)$barcode))

    ## None of the values of rowData(spe)$gene_search should be re-used in
    ## colData(spe)
    stopifnot(!all(rowData(spe)$gene_search %in% colnames(colData(spe))))

    ## No column named COUNT as that's used by vis_gene_p() and related
    ## functions.
    stopifnot(!"COUNT" %in% colnames(colData(spe)))

    ## The counts and logcounts assays
    stopifnot(all(c("counts", "logcounts") %in% assayNames(spe)))

    ## Done!
    return(spe)
}
