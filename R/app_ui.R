#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @import SingleCellExperiment
#' @importFrom DT DTOutput
#' @importFrom SummarizedExperiment assays
app_ui <- function() {
    ## Get options
    spe <- golem::get_golem_options("spe")
    docs_path <- golem::get_golem_options("docs_path")
    title <- golem::get_golem_options("title")

    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(docs_path),
        # List the first level UI elements here
        navbarPage(
            title = title,
            tabPanel("Overview", tagList(includeMarkdown(
                file.path(resourcePaths()["www"], "README.md")
            ))),
            tabPanel("spot-level data", tagList(
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = "sample",
                            label = "Sample to plot",
                            choices = unique(spe$sample_id)
                        ),
                        selectInput(
                            inputId = "cluster",
                            label = "Clusters to plot",
                            choices = c(
                                "spatialLIBD",
                                golem::get_golem_options("spe_discrete_vars")
                            )
                        ),
                        selectInput(
                            inputId = "reduced_name",
                            label = "Reduced dimensions",
                            choices = sort(reducedDimNames(spe)),
                            selected = reducedDimNames(spe)[length(reducedDimNames(spe))]
                        ),
                        pickerInput(
                            inputId = "geneid",
                            label = "Gene (or count variable)",
                            choices = c(
                                golem::get_golem_options("spe_continuous_vars"),
                                sort(rowData(spe)$gene_search)
                            ),
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        selectInput(
                            inputId = "assayname",
                            label = "Gene scale",
                            choices = c("counts", "logcounts"),
                            selected = "logcounts"
                        ),
                        numericInput(
                            inputId = "minCount",
                            label = "Minimum count value",
                            value = 0,
                            min = -1,
                            max = max(assays(spe)$logcounts),
                            step = 1
                        ),
                        selectInput(
                            inputId = "genecolor",
                            label = "Gene color scale",
                            choices = c("viridis", "paper"),
                            selected = "viridis"
                        ),
                        hr(),
                        checkboxInput("dropNA",
                            "Drop NA layer entries in the CSV file?",
                            value = TRUE
                        ),
                        downloadButton("downloadData", "Download layer guesses"),
                        helpText("Save your layer guesses frequently to avoid losing your work!"),
                        hr(),
                        fileInput(
                            "priorGuesses",
                            'Overwrite "Layer" with your prior guesses. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                            accept = c(
                                "text/csv",
                                ".csv",
                                "text/comma-separated-values,text/plain"
                            )
                        ),
                        helpText(
                            "This is useful for resuming your work. It should be a CSV file with the sample_id, spot_name, and layer columns."
                        ),
                        hr(),
                        width = 2
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                "Documentation",
                                tags$br(),
                                HTML(
                                    'Basic information overview about the spot-level SingleCellExperiment object. You can download it using <code>spatialLIBD::fetch_data(type = "spe")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput("raw_summary"),
                                helpText(
                                    "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
                                ),
                                tags$br(),
                                hr(),
                                includeMarkdown(file.path(
                                    resourcePaths()["www"], "documentation_spe.md"
                                )),
                                hr()
                            ),
                            tabPanel(
                                "Clusters (static)",
                                downloadButton("downloadPlotHistology", "Download PDF"),
                                plotOutput("histology"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Clusters (interactive)",
                                plotlyOutput("histology_plotly",
                                    width = "1200px",
                                    height = "1200px"
                                ),
                                textInput("label_layer", "Layer label", "Your Guess"),
                                checkboxInput(
                                    "label_click",
                                    "Enable layer-labelling by clicking on points",
                                    value = FALSE
                                ),
                                verbatimTextOutput("click"),
                                actionButton(
                                    "update_layer",
                                    "Label selected points (from lasso) with layer"
                                ),
                                helpText("Select points (lasso) to label them with a layer guess."),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Clusters grid (static)",
                                checkboxGroupInput(
                                    "grid_samples",
                                    label = "Select samples to show in the grid",
                                    choices = unique(spe$sample_id),
                                    selected = unique(spe$sample_id)[seq(1, 11, by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    "grid_nrow",
                                    label = "N rows",
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    "grid_ncol",
                                    label = "N columns",
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton("grid_update", label = "Update grid plot"),
                                downloadButton("downloadPlotHistologyGrid", "Download PDF"),
                                uiOutput("grid_static"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene (static)",
                                downloadButton("downloadPlotGene", "Download PDF"),
                                plotOutput("gene"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene (interactive)",
                                uiOutput("gene_plotly_cluster_subset_ui"),
                                plotlyOutput("gene_plotly",
                                    width = "600px",
                                    height = "600px"
                                ),
                                textInput("label_layer_gene", "Layer label", "Your Guess"),
                                checkboxInput(
                                    "label_click_gene",
                                    "Enable layer-labelling by clicking on points",
                                    value = FALSE
                                ),
                                verbatimTextOutput("click_gene"),
                                actionButton(
                                    "update_layer_gene",
                                    "Label selected points (from lasso) with layer"
                                ),
                                helpText("Select points to label them with a layer guess."),
                                helpText(
                                    "Note that only spots passing the minimum count value will be updated."
                                ),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene grid (static)",
                                checkboxGroupInput(
                                    "gene_grid_samples",
                                    label = "Select samples to show in the grid",
                                    choices = unique(spe$sample_id),
                                    selected = unique(spe$sample_id)[seq(1, length(unique(spe$sample_id)), by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    "gene_grid_nrow",
                                    label = "N rows",
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    "gene_grid_ncol",
                                    label = "N columns",
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton("gene_grid_update", label = "Update grid plot"),
                                downloadButton("downloadPlotGeneGrid", "Download PDF"),
                                uiOutput("gene_grid_static"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            )
                        )
                    )
                )
            )),
            uiOutput("spatial_layer_level"),
            tabPanel(
                "Help or feedback",
                tagList(
                    HTML(
                        'Please get in touch with the <code>spatialLIBD</code> authors through the <a href="https://support.bioconductor.org/">Bioconductor Support Website</a> (using the <code>spatialLIBD</code> <a href="https://support.bioconductor.org/t/spatialLIBD/">tag</a>) or through <a href="https://github.com/LieberInstitute/spatialLIBD/issues">GitHub</a>. Remember to help others help you by including all the information required to reproduce the problem you noticed. Thank you!'
                    ),
                    hr(),
                    p("The following information will be useful to them:"),
                    verbatimTextOutput("session_info")
                )
            ),
            hr(),
            tagList(
                includeHTML(
                    file.path(resourcePaths()["www"], "footer.html")
                )
            ),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),
            tags$br()
        )
    )
}

#' @import shiny
golem_add_external_resources <- function(docs_path) {
    addResourcePath(
        "www",
        docs_path
    )

    tags$head(
        golem::activate_js(),
        # Add here all the external resources
        # If you have a custom.css in the inst/app/www
        # Or for example, you can add shinyalert::useShinyalert() here
        # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        golem::favicon()
    )
}
