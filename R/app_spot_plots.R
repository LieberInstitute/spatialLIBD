# Spot-level documentation remains available beside the two visualization tabs.

app_spot_documentation_ui <- function(docs_path) {
    tabPanel(
        "Documentation",
        value = "documentation",
        tags$br(),
        HTML(
            'Basic information overview about the spot-level SingleCellExperiment object. You can download it using <code>spatialLIBD::fetch_data(type = "spe")</code>.'
        ),
        div(class = "app-plot-spacing"),
        verbatimTextOutput("raw_summary"),
        helpText(
            "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
        ),
        tags$br(),
        hr(),
        includeMarkdown(file.path(
            docs_path,
            "documentation_spe.md"
        )),
        hr()
    )
}
