# Connect the feature servers and establish each session's reactive dependencies.
# Combine edited images and manual labels into the SPE consumed by plots, then
# register spot-level features and the optional pseudobulk features.

#' @import shiny
#' @import SingleCellExperiment
#' @import ggplot2
#' @import grid
#' @import paletteer
#' @importFrom cowplot plot_grid
#' @importFrom viridisLite viridis
#' @importFrom sessioninfo session_info
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom grDevices as.raster pdf dev.off
#' @importFrom png readPNG
#' @importFrom scater plotReducedDim
#' @importFrom DT renderDT
#' @importFrom utils read.csv write.csv
#' @importFrom SummarizedExperiment assays

# Wire session-local images, annotations, and shared spot data into each feature server.
# Feature servers receive their data explicitly; each plotting tab owns an independent controls module.
app_server <- function(input, output, session) {
    options <- golem::get_golem_options()
    images <- app_image_editor_server(input, output, session, options$spe, options$datatype)
    annotations <- app_annotations_server(input, output, session, options$spe,
        gene_data = function() interactive$gene_data(),
        revision = function() interactive$revision(),
        sample = function() interactive_controls$input$sample
    )
    # Combine edited images and current labels without mutating the launch-time object.
    spe <- reactive({
        current <- images$spe()
        current$ManualAnnotation <- annotations()
        current
    })
    # Share data and image registration while keeping plotting controls independent.
    resources <- app_viewer_resources(session)
    catalog <- reactive(app_image_catalog(spe()))
    interactive_controls <- app_spot_controls_server("interactive_controls", spe, catalog)
    grid_controls <- app_spot_controls_server("grid_controls", spe, catalog, "grid")
    output$raw_summary <- renderPrint(print(spe()), width = 80)
    grids <- app_spot_grids_server(input, output, spe, grid_controls, catalog, session)
    interactive <- app_spot_interactive_server(input, output, session, spe, interactive_controls, resources, catalog)

    # Only initialize pseudobulk features when layer-level inputs were supplied.
    if (!is.null(options$sce_layer)) {
        sce <- options$sce_layer
        app_layer_controls_server(input, output, session, sce, options$sig_genes)
        dimensions <- app_layer_dimensions_server(input, output, session, sce)
        models <- app_layer_models_server(input, output, sce, options$sig_genes, options$default_cluster)
        enrichment <- app_layer_enrichment_server(input, output, sce, options$modeling_results, options$default_cluster)
        registration <- app_layer_registration_server(input, output, sce, options$modeling_results, options$default_cluster)
    }
    output$session_info <- renderPrint(sessioninfo::session_info(), width = 120)
    invisible(list(
        images = images, annotations = annotations, colors = colors,
        grids = grids, interactive = interactive,
        layers = if (!is.null(options$sce_layer)) {
            list(
                dimensions = dimensions, models = models,
                enrichment = enrichment, registration = registration
            )
        } else {
            NULL
        }
    ))
}
