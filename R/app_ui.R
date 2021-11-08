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
    spe_discrete_vars <-
        golem::get_golem_options("spe_discrete_vars")
    default_cluster <- golem::get_golem_options("default_cluster")
    sce_layer <- golem::get_golem_options("sce_layer")
    modeling_results <- golem::get_golem_options("modeling_results")
    sig_genes <- golem::get_golem_options("sig_genes")

    red_dim_names <- reducedDimNames(spe)
    if (length(red_dim_names) > 0) {
        red_dim_names <- sort(red_dim_names)
    } else {
        red_dim_names <- NULL
    }

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
                        helpText("The sample ID to visualize in most tabs except the 'grid' ones."),
                        hr(),
                        selectInput(
                            inputId = "cluster",
                            label = "Discrete variable to plot",
                            choices = spe_discrete_vars,
                            selected = default_cluster
                        ),
                        helpText("Typically cluster labels or any other discrete variable."),
                        hr(),
                        selectInput(
                            inputId = "reduced_name",
                            label = "Reduced dimensions",
                            choices = red_dim_names,
                            selected = red_dim_names[length(red_dim_names)]
                        ),
                        helpText("The first two dimensions are shown in the 'clusters (interactive)' tab."),
                        hr(),
                        pickerInput(
                            inputId = "geneid",
                            label = "Continuous variable to plot",
                            choices = c(
                                golem::get_golem_options("spe_continuous_vars"),
                                sort(rowData(spe)$gene_search)
                            ),
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        helpText("Typically a gene or any other continuous variable."),
                        hr(),
                        selectInput(
                            inputId = "assayname",
                            label = "Gene scale",
                            choices = assayNames(spe),
                            selected = assayNames(spe)[length(assayNames(spe))]
                        ),
                        helpText("The name of the gene values you want to visualize."),
                        hr(),
                        selectInput(
                            inputId = "imageid",
                            label = "Image name",
                            choices = unique(imgData(spe)$image_id),
                            selected = unique(imgData(spe)$image_id)[1]
                        ),
                        helpText("The name of the background image you want to visualize."),
                        hr(),
                        numericInput(
                            inputId = "minCount",
                            label = "Minimum count value",
                            value = 0,
                            min = -1,
                            max = max(assays(spe)[[length(assayNames(spe))]]),
                            step = 1
                        ),
                        helpText(
                            "You can manually enter any number then press enter in your keyboard. This is useful for extreme values."
                        ),
                        hr(),
                        selectInput(
                            inputId = "genecolor",
                            label = "Gene color scale",
                            choices = c("viridis", "paper"),
                            selected = "viridis"
                        ),
                        helpText("The viridis scale is color-blind friendly."),
                        hr(),
                        checkboxInput("dropNA",
                            "Drop NA layer entries in the CSV file?",
                            value = TRUE
                        ),
                        downloadButton("downloadData", "Download manual annotations"),
                        helpText(
                            "Save your manual annotations frequently to avoid losing your work!"
                        ),
                        hr(),
                        fileInput(
                            "priorGuesses",
                            'Overwrite "ManualAnnotation" with your prior annotations. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                            accept = c(
                                "text/csv",
                                ".csv",
                                "text/comma-separated-values,text/plain"
                            )
                        ),
                        helpText(
                            "This is useful for resuming your work. It should be a CSV file with the sample_id, spot_name, and ManualAnnotation columns."
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
                                checkboxInput(
                                    "side_by_side_histology",
                                    "Show clusters and background image side by side.",
                                    value = FALSE
                                ),
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
                                helpText("This is a 2 by 2 plotting area with the top row showing the spatially-resolved data on top of the histology images. The bottom row shows the data on the reduced dimensions. The left column shows the selected continuous variable (typically a gene) while the right column shows the selected discreate variable (typically cluster annotations)."),
                                textInput("label_manual_ann", "Manual annotation label", "Your Guess"),
                                checkboxInput(
                                    "label_click",
                                    "Enable spot manual annotations by clicking on individual spots (double left-mouse click).",
                                    value = FALSE
                                ),
                                verbatimTextOutput("click"),
                                actionButton(
                                    "update_manual_ann",
                                    "Label selected points (from lasso) with manual annotation"
                                ),
                                helpText("Select points (lasso) to label them with a manual annotation."),
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
                                helpText("Click the 'upgrade grid plot' button above to re-make this plot."),
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
                                checkboxInput(
                                    "side_by_side_gene",
                                    "Show gene and background image side by side.",
                                    value = FALSE
                                ),
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
                                textInput(
                                    "label_manual_ann_gene",
                                    "Manual annotation label",
                                    "Your Guess"
                                ),
                                checkboxInput(
                                    "label_click_gene",
                                    "Enable spot manual annotations by clicking on individual spots (double left-mouse click).",
                                    value = FALSE
                                ),
                                verbatimTextOutput("click_gene"),
                                actionButton(
                                    "update_manual_ann_gene",
                                    "Label selected points (from lasso) with manual annotation"
                                ),
                                helpText("Select points to label them with a manual annotation."),
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
                                helpText("Click the 'upgrade grid plot' button above to re-make this plot."),
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
            if (!is.null(sce_layer)) {
                tabPanel("layer-level data", tagList(sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = "layer_model",
                            label = "Model results",
                            choices = c("enrichment", "pairwise", "anova"),
                            selected = "enrichment"
                        ),
                        hr(),
                        pickerInput(
                            inputId = "layer_geneid",
                            label = "Gene",
                            choices =
                                sort(rowData(sce_layer)$gene_search),
                            options = pickerOptions(liveSearch = TRUE)
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
                                    'Basic information overview about the layer-level SingleCellExperiment object (pseudo-bulked from the spot-level data). You can download it using <code>spatialLIBD::fetch_data(type = "sce_layer")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput("layer_raw_summary"),
                                helpText(
                                    "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
                                ),
                                tags$br(),
                                hr(),
                                includeMarkdown(
                                    file.path(resourcePaths()["www"], "documentation_sce_layer.md")
                                ),
                            ),
                            tabPanel(
                                "Reduced Dim",
                                selectInput(
                                    inputId = "layer_which_dim",
                                    label = "Reduced Dimension",
                                    choices = sort(reducedDimNames(sce_layer)),
                                    selected = "PCA"
                                ),
                                selectInput(
                                    inputId = "layer_which_dim_color",
                                    label = "Color by",
                                    choices = sort(colnames(colData(sce_layer))),
                                    selected = default_cluster
                                ),
                                downloadButton("layer_downloadReducedDim", "Download PDF"),
                                plotOutput("layer_reduced_dim"),
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
                                "Model boxplots",
                                selectInput(
                                    inputId = "layer_model_test",
                                    label = "Model test",
                                    choices = sort(unique(sig_genes$test[sig_genes$model_type == "enrichment"]))
                                ),
                                selectInput(
                                    inputId = "layer_boxcolor",
                                    label = "Boxplot color scale",
                                    choices = c("viridis", "paper", "bluered"),
                                    selected = "viridis"
                                ),
                                checkboxInput(
                                    "layer_box_shortitle",
                                    "Enable short title on boxplots.",
                                    value = TRUE
                                ),
                                hr(),
                                downloadButton("layer_downloadBoxplot", "Download PDF"),
                                plotOutput("layer_boxplot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                downloadButton("layer_downloadModelTable", "Download CSV"),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_model_table")
                            ),
                            tabPanel(
                                "Gene Set Enrichment",
                                fileInput(
                                    "geneSet",
                                    "Upload a CSV file with one column per gene set with a header row and then Ensembl gene IDs as values.",
                                    accept = c(
                                        "text/csv",
                                        ".csv",
                                        "text/comma-separated-values,text/plain"
                                    )
                                ),
                                helpText(
                                    "It should be a CSV file without row names and similar to ",
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/asd_sfari_geneList.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    "layer_gene_fdrcut",
                                    label = "FDR cutoff",
                                    value = 0.1,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                hr(),
                                downloadButton("layer_downloadGeneSet", "Download PDF"),
                                plotOutput("layer_gene_set_plot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                downloadButton("layer_downloadGeneSetTable", "Download CSV"),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_gene_set_table")
                            ),
                            tabPanel(
                                "Spatial registration",
                                fileInput(
                                    "externalTstat",
                                    "Upload a CSV file with one column per cell type or layer that contains the enrichment t-stat equivalent and with Ensembl gene IDs as the row names.",
                                    accept = c(
                                        "text/csv",
                                        ".csv",
                                        "text/comma-separated-values,text/plain"
                                    )
                                ),
                                helpText(
                                    "It should be a CSV file similar to ",
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    "layer_tstat_max",
                                    label = "Maximum correlation",
                                    value = 0.81,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                helpText("Use a smaller positive number to change the range of the color scale used."),
                                hr(),
                                downloadButton("layer_downloadTstatCor", "Download PDF"),
                                plotOutput("layer_tstat_cor_plot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                downloadButton("layer_downloadTstatCorTable", "Download CSV"),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_tstat_cor_table")
                            )
                        )
                    )
                )))
            } else {
                NULL
            },
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
            footer = tagList(
                hr(),
                includeHTML(
                    file.path(resourcePaths()["www"], "footer.html")
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
