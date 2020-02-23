#' Run the spatialLIBD Shiny Application
#'
#' This function runs the shiny application that allows users to interact
#' with the Visium spatial transcriptomics data from LIBD (by default) or
#' any other data that you have shaped according to our object structure.
#'
#' @param sce Defaults to the output of
#' `fetch_data(type = 'sce')`. This is a
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class]
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
#' if(interactive()) {
#'     ## The default arguments will download the data from the web
#'     ## using fetch_data(). If this is the first time you have run this,
#'     ## the files will need to be cached by ExperimentHub. Otherwise it
#'     ## will re-use the files you have previously downloaded.
#'     run_app()
#' }
#'

run_app <- function(sce = fetch_data(type = 'sce'),
    sce_layer = fetch_data(type = 'sce_layer'),
    modeling_results = fetch_data(type = 'modeling_results'),
    sig_genes = sig_genes_extract_all(
        n = nrow(sce_layer),
        modeling_results = modeling_results,
        sce_layer = sce_layer
    ),
    image_path = system.file('app/www/data', package = 'spatialLIBD'),
    ...) {
    with_golem_options(
        app = shinyApp(ui = app_ui, server = app_server),
        golem_opts = list(
            sce = sce,
            sce_layer = sce_layer,
            modeling_results = modeling_results,
            image_path = image_path,
            sig_genes = sig_genes,
            ...
        )
    )

}
