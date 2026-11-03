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

# Feature servers receive their data explicitly. IDs stay at app scope so existing
# bookmarks, automation, and downstream app integrations keep their names.
app_server <- function(input, output, session) {
    options <- golem::get_golem_options()
    images <- app_image_editor_server(input, output, session, options$spe, options$datatype)
    annotations <- app_annotations_server(input, output, session, options$spe,
        gene_data = function() interactive$gene_data()
    )
    spe <- reactive({
        current <- images$spe()
        current$ManualAnnotation <- annotations()
        current
    })
    colors <- app_spot_controls_server(input, session, spe)
    settings <- reactive(app_spot_settings(input, options$is_stitched, options$datatype))
    plots <- app_spot_plots_server(input, output, spe, colors, settings)
    grids <- app_spot_grids_server(input, output, spe, colors, settings)
    interactive <- app_spot_interactive_server(input, output, spe, colors, settings)

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
        plots = plots, grids = grids, interactive = interactive,
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
