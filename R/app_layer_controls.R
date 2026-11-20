# Build the pseudobulk sidebar, documentation, and object summary.
# Coordinate model, test, and gene choices so dependent selections remain valid
# as the user moves between the supplied statistical results.

# Expose the statistical model family shared by the layer-level result tabs.
app_layer_controls_ui <- function(modeling_results, default_model_type) {
    sidebarPanel(
        selectInput(
            inputId = "layer_model",
            label = "Model results",
            choices = names(modeling_results),
            selected = default_model_type
        ),
        hr(),
        width = 2
    )
}

# Show the pseudobulk object summary and the layer-level usage documentation.
app_layer_documentation_ui <- function(docs_path) {
    tabPanel(
        "Documentation",
        tags$br(),
        HTML(
            'Basic information overview about the layer-level SingleCellExperiment object (pseudo-bulked from the spot-level data). You can download it using <code>spatialLIBD::fetch_data(type = "sce_layer")</code>.'
        ),
        div(class = "app-plot-spacing"),
        verbatimTextOutput("layer_raw_summary"),
        helpText(
            "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
        ),
        tags$br(),
        hr(),
        includeMarkdown(
            file.path(
                docs_path,
                "documentation_sce_layer.md"
            )
        ),
    )
}

# Keep test and gene choices valid as the selected model or comparison changes.
app_layer_controls_server <- function(input, output, session, sce, sig_genes) {
    output$layer_raw_summary <- renderPrint(print(sce), width = 80)
    # Derive available comparisons from the selected model's gene-result table.
    tests <- reactive({
        req(input$layer_model)
        sort(unique(sig_genes$test[sig_genes$model_type == input$layer_model]))
    })
    observeEvent(input$layer_model, {
        choices <- tests()
        updatePickerInput(session, "layer_model_test",
            choices = choices,
            selected = app_preferred(choices, isolate(input$layer_model_test)),
            options = pickerOptions(liveSearch = TRUE)
        )
    })
    # Preserve the chosen gene when possible, otherwise select a valid replacement.
    observeEvent(list(input$layer_model, input$layer_model_test), {
        req(input$layer_model, input$layer_model_test)
        rows <- sig_genes$model_type == input$layer_model & sig_genes$test == input$layer_model_test
        choices <- sort(unique(rowData(sce)$gene_search[sig_genes$gene_index[rows]]))
        updatePickerInput(session, "layer_geneid",
            choices = choices,
            selected = app_preferred(choices, isolate(input$layer_geneid)),
            options = pickerOptions(liveSearch = TRUE)
        )
    })
    invisible(NULL)
}
