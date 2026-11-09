# Compose the app's navigation, spot-level and pseudobulk pages, help, and resources.
# Read launch options here and pass them to the feature UI functions; individual
# controls and layouts live alongside their feature's server code.

#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput
#' @import SingleCellExperiment
#' @importFrom DT DTOutput
#' @importFrom SummarizedExperiment assays
app_ui <- function() {
    options <- golem::get_golem_options()
    tagList(
        golem_add_external_resources(options$docs_path),
        navbarPage(
            title = options$title,
            tabPanel("Overview", includeMarkdown(app_document(options$docs_path, "README.md"))),
            tabPanel("spot-level data", sidebarLayout(
                app_spot_controls_ui(
                    options$spe, options$spe_discrete_vars, options$spe_continuous_vars,
                    options$default_cluster, options$auto_crop_default, options$datatype
                ),
                mainPanel(tabsetPanel(
                    id = "spot_tabs",
                    app_spot_documentation_ui(options$docs_path),
                    app_cluster_plot_ui(),
                    app_cluster_interactive_ui(),
                    app_cluster_grid_ui(options$spe),
                    app_gene_plot_ui(),
                    app_gene_interactive_ui(),
                    app_gene_grid_ui(options$spe),
                    if (options$datatype != "Xenium") app_image_editor_ui()
                ))
            )),
            if (!is.null(options$sce_layer)) app_layer_ui(options),
            app_help_ui(),
            id = "app_tabs",
            footer = tagList(hr(), includeHTML(app_document(options$docs_path, "footer.html")))
        )
    )
}

app_layer_ui <- function(options) {
    default_model <- app_preferred(names(options$modeling_results), "enrichment")
    default_dim <- app_preferred(reducedDimNames(options$sce_layer), "PCA")
    tabPanel("layer-level data", sidebarLayout(
        app_layer_controls_ui(options$modeling_results, default_model),
        mainPanel(tabsetPanel(
            id = "layer_tabs",
            app_layer_documentation_ui(options$docs_path),
            app_layer_dimensions_ui(options$sce_layer, default_dim, options$default_cluster),
            app_layer_models_ui(options$sce_layer, options$sig_genes, default_model),
            app_layer_enrichment_ui(),
            app_layer_registration_ui()
        ))
    ))
}

app_help_ui <- function() {
    tabPanel("Help or feedback", tagList(
        HTML(paste0(
            "Please get in touch with the <code>spatialLIBD</code> authors through the ",
            '<a href="https://support.bioconductor.org/">Bioconductor Support Website</a> ',
            "(using the <code>spatialLIBD</code> ",
            '<a href="https://support.bioconductor.org/tag/spatialLIBD/">tag</a>) or through ',
            '<a href="https://github.com/LieberInstitute/spatialLIBD/issues">GitHub</a>. ',
            "Include the steps and data needed to reproduce the problem."
        )),
        hr(), p("The following information will be useful to them:"),
        verbatimTextOutput("session_info")
    ))
}

app_document <- function(docs_path, name) file.path(docs_path, name)

#' @import shiny
golem_add_external_resources <- function(docs_path) {
    addResourcePath("www", docs_path)
    tags$head(
        golem::activate_js(),
        golem::favicon(),
        tags$style(HTML(".app-plot-spacing { height: 2rem; }"))
    )
}
