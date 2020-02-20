#' Run the Shiny Application
#'
#' @param sce
#' @param sce_layer
#' @param modeling_results
#' @param image_path
#' @param ...
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(sce = fetch_data(type = 'sce'),
    sce_layer = fetch_data(type = 'sce_layer'),
    modeling_results = fetch_data(type = 'modeling_results'),
    image_path = system.file('app/www/data', package = 'spatialLIBD'),
    sig_genes = sig_genes_extract_all(n = nrow(sce_layer), modeling_results = modeling_results),
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
