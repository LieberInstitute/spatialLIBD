#' Run the spatialLIBD Shiny Application
#'
#' This function runs the shiny application that allows users to interact
#' with the Visium spatial transcriptomics data from LIBD (by default) or
#' any other data that you have shaped according to our object structure.
#'
#' If you don't have the pseudo-bulked analysis results like we computed them
#' in our project <https://doi.org/10.1038/s41593-020-00787-0> you can
#' set `sce_layer`, `modeling_results` and `sig_genes` to `NULL`. Doing so
#' will disable the pseudo-bulked portion of the web application. See the
#' examples for one such case as well as the vignette that describes how
#' you can use `spatialLIBD` with public data sets provided by 10x Genomics.
#' That vignette is available at
#' <http://research.libd.org/spatialLIBD/articles/TenX_data_download.html>.
#'
#' @param spe Defaults to the output of
#' `fetch_data(type = 'spe')`. This is a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object with the spot-level Visium data and information required for
#' visualizing the histology. See [fetch_data()] for more details.
#' @inheritParams sig_genes_extract
#' @param sig_genes The output of [sig_genes_extract_all()] which is a table
#' in long format with the modeling results.
#' @param docs_path A `character(1)` specifying the path to the directory
#' containing the website documentation files. The directory has to contain
#' the files: `documentation_sce_layer.md`, `documentation_spe.md`,
#' `favicon.ico`, `footer.html` and `README.md`.
#' @param title A character(1) specifying the title for the app.
#' @param spe_discrete_vars A `character()` vector of discrete variables that
#' will be available to visualize in the app. Basically, the set of variables
#' with spot-level groups. They will have to be present in `colData(spe)`.
#' @param spe_continuous_vars A `character()` vector of continuous variables
#' that will be available to visualize in the app using the same scale
#' as genes. They will have to be present in `colData(sce)`.
#' @param default_cluster A `character(1)` with the name of the main cluster
#' (discrete) variable to use. It will have to be present in both `colData(spe)`
#' and `colData(sce_layer)`.
#' @param ... Other arguments passed to the list of golem options for running
#' the application.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @return A [shiny.appobj][shiny::shiny.appobj] that contains the input data.
#'
#' @examples
#' \dontrun{
#' ## The default arguments will download the data from the web
#' ## using fetch_data(). If this is the first time you have run this,
#' ## the files will need to be cached by ExperimentHub. Otherwise it
#' ## will re-use the files you have previously downloaded.
#' if (enough_ram(4e9)) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Create the interactive website
#'     run_app(spe)
#'
#'     ## You can also run a custom version without the pseudo-bulked
#'     ## layer information. This is useful if you are only interested
#'     ## in the spatial transcriptomics features.
#'     run_app(spe,
#'         sce_layer = NULL, modeling_results = NULL, sig_genes = NULL,
#'         title = "spatialLIBD without layer info"
#'     )
#' }
#' }
run_app <- function(spe = fetch_data(type = "spe"),
    sce_layer = fetch_data(type = "sce_layer"),
    modeling_results = fetch_data(type = "modeling_results"),
    sig_genes = sig_genes_extract_all(
        n = nrow(sce_layer),
        modeling_results = modeling_results,
        sce_layer = sce_layer
    ),
    docs_path = system.file("app", "www", package = "spatialLIBD"),
    title = "spatialLIBD",
    spe_discrete_vars = c(
        "spatialLIBD",
        "GraphBased",
        "ManualAnnotation",
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
    spe_continuous_vars = c(
        "cell_count",
        "sum_umi",
        "sum_gene",
        "expr_chrM",
        "expr_chrM_ratio"
    ),
    default_cluster = "spatialLIBD",
    ...) {
    ## Run the checks in the relevant order
    stopifnot(length(default_cluster) == 1)
    stopifnot(default_cluster %in% spe_discrete_vars)

    spe <-
        check_spe(spe,
            variables = c(spe_discrete_vars, spe_continuous_vars)
        )

    ## Check sce_layer and modeling_results if needed
    if (!is.null(sce_layer)) {
        sce_layer <-
            check_sce_layer(sce_layer, variables = default_cluster)
        modeling_results <- check_modeling_results(modeling_results)
        ## No need to check sig_genes since sig_genes_extract_all() will fail
    }

    ## Check that the required documentation files exist
    stopifnot(all(file.exists(file.path(
        docs_path,
        c(
            "documentation_spe.md",
            "favicon.ico",
            "footer.html",
            "README.md"
        )
    ))))
    if (!is.null(sce_layer)) {
        ## Check required files when sce_layer is present
        stopifnot(file.exists(file.path(
            docs_path, "documentation_sce_layer.md"
        )))
    }

    with_golem_options(
        app = shinyApp(ui = app_ui, server = app_server),
        golem_opts = list(
            spe = spe,
            sce_layer = sce_layer,
            modeling_results = modeling_results,
            sig_genes = sig_genes,
            docs_path = docs_path,
            title = title,
            spe_discrete_vars = spe_discrete_vars,
            spe_continuous_vars = spe_continuous_vars,
            default_cluster = default_cluster,
            ...
        )
    )
}
