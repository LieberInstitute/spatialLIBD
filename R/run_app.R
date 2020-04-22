#' Run the spatialLIBD Shiny Application
#'
#' This function runs the shiny application that allows users to interact
#' with the Visium spatial transcriptomics data from LIBD (by default) or
#' any other data that you have shaped according to our object structure.
#'
#' @param sce Defaults to the output of
#' `fetch_data(type = 'sce')`. This is a
#' \linkS4class{SingleCellExperiment}
#' object with the spot-level Visium data and information required for
#' visualizing the histology. See [fetch_data()] for more details.
#' @inheritParams sig_genes_extract
#' @param sig_genes The output of [sig_genes_extract_all()] which is a table
#' in long format with the modeling results.
#' @param image_path A path to the directory containing the low resolution
#' histology images that is needed for the interactive visualizations with
#' `plotly`. See
#' https://github.com/LieberInstitute/spatialLIBD/tree/master/inst/app/www/data
#' for an example of how these files should be organized.
#' @param sce_discrete_vars A `character()` vector of discrete variables that
#' will be available to visualize in the app. Basically, the set of variables
#' with spot-level groups. They will have to be present in `colData(sce)`.
#' @param sce_continuous_vars A `character()` vector of continuous variables
#' that will be available to visualize in the app using the same scale
#' as genes. They will have to be present in `colData(sce)`.
#' @param spatial_libd_var A `character(1)` with the name of the main cluster
#' variable to use. It will have to be present in both `colData(sce)` and
#' `colData(sce_layer)`.
#' @param ... Other arguments passed to the list of golem options for running
#' the application.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @return A [shiny.appobj][shiny::shiny.appobj] that contains the input data.
#'
#' @examples
#'
#' \dontrun{
#' ## The default arguments will download the data from the web
#' ## using fetch_data(). If this is the first time you have run this,
#' ## the files will need to be cached by ExperimentHub. Otherwise it
#' ## will re-use the files you have previously downloaded.
#' run_app()
#' }
#'
run_app <- function(sce = fetch_data(type = "sce"),
    sce_layer = fetch_data(type = "sce_layer"),
    modeling_results = fetch_data(type = "modeling_results"),
    sig_genes = sig_genes_extract_all(
        n = nrow(sce_layer),
        modeling_results = modeling_results,
        sce_layer = sce_layer
    ),
    image_path = system.file("app", "www", "data", package = "spatialLIBD"),
    sce_discrete_vars = c(
        "GraphBased",
        "Layer",
        "Maynard",
        "Martinowich",
        paste0("SNN_k50_k", 4:28),
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
    ),
    sce_continuous_vars = c(
        "cell_count",
        "sum_umi",
        "sum_gene",
        "expr_chrM",
        "expr_chrM_ratio"
    ),
    spatial_libd_var = "layer_guess_reordered_short",
    ...) {
    ## Run the checks in the relevant order
    stopifnot(length(spatial_libd_var) == 1)
    sce <-
        check_sce(sce,
            variables = c(spatial_libd_var, sce_discrete_vars, sce_continuous_vars)
        )
    if (!exists("sce_layer")) {
        sce_layer <-
            check_sce_layer(sce_layer, variables = spatial_libd_var)
    }
    modeling_results <- check_modeling_results(modeling_results)
    image_path <- check_image_path(image_path, sce)
    ## No need to check sig_genes since sig_genes_extract_all() will fail

    with_golem_options(
        app = shinyApp(ui = app_ui, server = app_server),
        golem_opts = list(
            sce = sce,
            sce_layer = sce_layer,
            modeling_results = modeling_results,
            image_path = image_path,
            sig_genes = sig_genes,
            sce_discrete_vars = sce_discrete_vars,
            sce_continuous_vars = sce_continuous_vars,
            spatial_libd_var = spatial_libd_var,
            ...
        )
    )
}
