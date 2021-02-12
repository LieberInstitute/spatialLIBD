#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @import SingleCellExperiment
#' @importFrom DT DTOutput
#' @importFrom SummarizedExperiment assays
app_ui <- function() {
    ## Get options
    spe <- golem::get_golem_options("spe")
    sce_layer <- golem::get_golem_options("sce_layer")
    image_path <- golem::get_golem_options("image_path")
    sig_genes <- golem::get_golem_options("sig_genes")
    spatial_libd_var <- golem::get_golem_options("spatial_libd_var")

    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(image_path),
        # List the first level UI elements here
        navbarPage(
            title = "spatialLIBD",
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
            tabPanel("layer-level data", tagList(
                sidebarLayout(
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
                                    selected = spatial_libd_var
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
                                "stat correlation",
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
                )
            )),
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
                HTML(
                    'This <a href="https://shiny.rstudio.com/">shiny</a> application was developed by the <a href="http://aejaffe.com/">Data Science Team I</a> at the <a href="https://www.libd.org/">Lieber Institute for Brain Development</a> and is hosted by LIBD, with additional mirrors at shinyapps provided thanks to the <a href="https://www.jhsph.edu/departments/biostatistics/">Department of Biostatistics at the Johns Hopkins Bloomberg School of Public Health</a>. It is powered by the <code>spatialLIBD</code> R package which you can find described in <a href="http://research.libd.org/spatialLIBD/">its documentation website</a> and you can use to run locally this shiny application by running the command <code>spatialLIBD::run_app()</code>.'
                ),
                hr(),
                HTML(
                    'If you tweet about this website, the data or the R package please use the <code>#spatialLIBD</code> hashtag. You can find previous tweets that way as shown <a href="https://twitter.com/search?q=%23spatialLIBD&src=typed_query">here</a>. Thank you! <a href="https://twitter.com/intent/tweet?button_hashtag=spatialLIBD&ref_src=twsrc%5Etfw" class="twitter-hashtag-button" data-show-count="false">Tweet #spatialLIBD</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>'
                ),
                hr(),
                HTML(
                    'The <code>spatialLIBD</code> R package is currently under review at <a href="https://github.com/Bioconductor/Contributions/issues/1389">Bioconductor</a>.'
                ),
                hr(),
                HTML('<a href="http://www.libd.org/">'),
                img(
                    src = "http://lcolladotor.github.io/img/LIBD_logo.jpg",
                    align = "left",
                    width = "250"
                ),
                HTML("</a>"),
                HTML(
                    "<center>
      <script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=300&t=n&d=O0-J9HIlFG7Lp9zsQk6P8Uqz98Ny3K8cygM4qhvJabQ'></script>
      </center>"
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
golem_add_external_resources <- function(image_path) {
    addResourcePath(
        "www",
        system.file("app", "www", package = "spatialLIBD")
    )
    addResourcePath("imagedata", image_path)

    tags$head(
        golem::activate_js(),
        # Add here all the external resources
        # If you have a custom.css in the inst/app/www
        # Or for example, you can add shinyalert::useShinyalert() here
        # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        tags$head(
            includeScript(
                file.path(resourcePaths()["www"], "gtag.js"),
                'async src="https://www.googletagmanager.com/gtag/js?id=UA-159132967-1"'
            )
        ),
        golem::favicon()
    )
}
