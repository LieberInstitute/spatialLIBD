#' Check input sce
#'
#' This function checks that the `sce` object has the appropriate structure.
#' For more details please check the vignette documentation.
#'
#' @inheritParams run_app
#' @param variables A `character()` vector of variable names expected to
#' be present in `colData(sce)`.
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @importFrom SummarizedExperiment assayNames
#' @family Check input functions
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists('ori_sce')) ori_sce <- fetch_data('sce')
#'
#' ## Check the object
#' check_sce(ori_sce)
#'

check_sce <- function(sce,
    variables = c(
        'Cluster10X',
        'Layer',
        'Maynard',
        'Martinowich',
        paste0('SNN_k50_k', 4:28),
        'layer_guess_reordered_short',
        'cell_count',
        'sum_umi',
        'sum_gene',
        'expr_chrM',
        'expr_chrM_ratio'
    )) {
    ## Should be a SingleCellExperiment object
    stopifnot(is(sce, 'SingleCellExperiment'))

    ## Images data stored under metadata(sce)$image
    stopifnot('image' %in% names(metadata(sce)))
    stopifnot(all(c('sample', 'grob') %in% colnames(metadata(sce)$image)))

    ## Ensembl gene IDs are rownames with the symbol (gene_name) and Ensembl
    ## ID (gene_name) pasted into `gene_search`
    stopifnot(all(
        c('gene_id', 'gene_name', 'gene_search') %in% colnames(rowData(sce))
    ))
    stopifnot(identical(
        paste0(rowData(sce)$gene_name, '; ', rowData(sce)$gene_id),
        rowData(sce)$gene_search
    ))

    ## Rownames are Ensembl ids
    stopifnot(identical(rownames(sce), rowData(sce)$gene_id))

    ## Information about the image coordinates stored in imagerow and imagecol
    ## The sample names stored under sce$sample_name

    ## Some cluster variables, though you could potentially edit
    ## spatialLIBD::app_ui()
    ## or this could be made into a run_app() argument.

    ## Some continuous variables
    stopifnot(all(
        c('imagerow',
            'imagecol',
            'sample_name',
            'key',
            variables) %in% colnames(colData(sce))
    ))

    ## A unique spot-level ID (such as barcode) stored under sce$key
    ## The 'key' column is necessary for the plotly code to work.
    stopifnot(length(unique(sce$key)) == ncol(sce))
    # identical(sce$key, paste0(sce$sample_name, '_', sce$barcode))

    ## None of the values of rowData(sce)$gene_search should be re-used in
    ## colData(sce)
    stopifnot(!all(rowData(sce)$gene_search %in% colnames(colData(sce))))

    ## No column named COUNT as that's used by sce_image_gene_p() and related
    ## functions.
    stopifnot(!'COUNT' %in% colnames(colData(sce)))

    ## The counts and logcounts assays
    stopifnot(all(c('counts', 'logcounts') %in% assayNames(sce)))

    ## Done!
    return(sce)
}
