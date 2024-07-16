#' Check input sce_layer
#'
#' This function checks that the `sce_layer` object has the appropriate structure.
#' For more details please check the vignette documentation.
#'
#' @inheritParams run_app
#' @param variables A `character()` vector of variable names expected to
#' be present in `colData(sce_layer)`.
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @family Check input functions
#'
#' @examples
#'
#' ## Obtain example data from the HumanPilot project
#' ## (Maynard, Collado-Torres, et al, 2021)
#' if (!exists("sce_layer")) sce_layer <- fetch_data("sce_layer")
#'
#' ## Check the pseudo-bulked data
#' check_sce_layer(sce_layer)
check_sce_layer <- function(sce_layer, variables = "spatialLIBD") {
    ## Should be a SingleCellExperiment object
    stopifnot(is(sce_layer, "SingleCellExperiment"))

    ## For the layer-level data, some of the shiny code is tailored to our data
    ## though you could change it. Hence these requirements are fairly specific.
    stopifnot(all(variables %in% colnames(colData(sce_layer))))

    ## Ensembl gene IDs are rownames with the symbol (gene_name) and Ensembl
    ## ID (gene_name) pasted into `gene_search`
    stopifnot(all(
        c("gene_id", "gene_name", "gene_search") %in% colnames(rowData(sce_layer))
    ))
    stopifnot(identical(
        paste0(rowData(sce_layer)$gene_name, "; ", rowData(sce_layer)$gene_id),
        rowData(sce_layer)$gene_search
    ))

    ## Rownames are Ensembl ids
    stopifnot(identical(rownames(sce_layer), rowData(sce_layer)$gene_id))

    ## Done!
    return(sce_layer)
}
