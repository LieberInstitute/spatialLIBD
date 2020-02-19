#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @import SingleCellExperiment
app_ui <- function() {
    ## Get options
    sce <- golem::get_golem_options('sce')
    image_path <- golem::get_golem_options('image_path')

    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(image_path),
        # List the first level UI elements here
        navbarPage(
            title = 'spatialLIBD',
            tabPanel('spot-level data', tagList(
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = 'sample',
                            label = 'Sample to plot',
                            choices = unique(sce$sample_name)
                        ),
                        selectInput(
                            inputId = 'cluster',
                            label = 'Clusters to plot',
                            choices = c(
                                'Cluster10X',
                                'Layer',
                                'layer_guess',
                                'layer_guess_reordered',
                                'layer_guess_reordered_short',
                                'Maynard',
                                'Martinowich',
                                paste0('SNN_k50_k', 4:28)
                            )
                        ),
                        selectInput(
                            inputId = 'reduced_name',
                            label = 'Reduced dimensions',
                            choices = reducedDimNames(sce)[-1],
                            selected = reducedDimNames(sce)[length(reducedDimNames(sce))]
                        ),
                        pickerInput(
                            inputId = 'geneid',
                            label = 'Gene (or count variable)',
                            choices = c(
                                'cell_count',
                                'sum_umi',
                                'sum_gene',
                                'expr_chrM',
                                'expr_chrM_ratio',
                                sort(paste0(
                                    rowData(sce)$gene_name, '; ', rowData(sce)$gene_id
                                ))
                            ),
                            selected = 'cell_count',
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        selectInput(
                            inputId = 'assayname',
                            label = 'Gene scale',
                            choices = c('counts', 'logcounts'),
                            selected = 'logcounts'
                        ),
                        numericInput(
                            inputId = 'minCount',
                            label = 'Minimum count value',
                            value = 0,
                            min = -1,
                            max = max(assays(sce)$logcounts),
                            step = 1
                        ),
                        selectInput(
                            inputId = 'genecolor',
                            label = 'Gene color scale',
                            choices = c('viridis', 'paper'),
                            selected = 'viridis'
                        ),
                        hr(),
                        checkboxInput('dropNA',
                            'Drop NA layer entries in the CSV file?',
                            value = TRUE),
                        downloadButton('downloadData', 'Download layer guesses'),
                        helpText('Save your layer guesses frequently to avoid losing your work!'),
                        hr(),
                        fileInput(
                            'priorGuesses',
                            'Overwrite "Layer" with your prior guesses. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                            accept = c(
                                'text/csv',
                                '.csv',
                                'text/comma-separated-values,text/plain'
                            )
                        ),
                        helpText(
                            'This is useful for resuming your work. It should be a CSV file with the sample_name, spot_name, and layer columns.'
                        ),
                        hr(),
                        width = 2
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel('Raw summary',
                                verbatimTextOutput('raw_summary')),
                            tabPanel(
                                'Clusters (static)',
                                downloadButton('downloadPlotHistology', 'Download PDF'),
                                plotOutput('histology'),
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
                                'Clusters (interactive)',
                                plotlyOutput('histology_plotly',
                                    width = '1200px',
                                    height = '1200px'),
                                plotlyOutput('histology_plotly_gene'),
                                textInput('label_layer', 'Layer label', 'Your Guess'),
                                checkboxInput(
                                    'label_click',
                                    'Enable layer-labelling by clicking on points',
                                    value = FALSE
                                ),
                                verbatimTextOutput("click"),
                                actionButton(
                                    'update_layer',
                                    'Label selected points (from lasso) with layer'
                                ),
                                helpText('Select points (lasso) to label them with a layer guess.'),
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
                                'Clusters grid (static)',
                                checkboxGroupInput(
                                    'grid_samples',
                                    label = 'Select samples to show in the grid',
                                    choices = unique(sce$sample_name),
                                    selected = unique(sce$sample_name)[seq(1, 11, by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    'grid_nrow',
                                    label = 'N rows',
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    'grid_ncol',
                                    label = 'N columns',
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton('grid_update', label = 'Update grid plot'),
                                downloadButton('downloadPlotHistologyGrid', 'Download PDF'),
                                uiOutput('grid_static'),
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
                                'Gene (static)',
                                downloadButton('downloadPlotGene', 'Download PDF'),
                                plotOutput('gene'),
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
                                'Gene (interactive)',
                                uiOutput('gene_plotly_cluster_subset_ui'),
                                plotlyOutput('gene_plotly',
                                    width = '600px',
                                    height = '600px'),
                                plotlyOutput('gene_plotly_clusters'),
                                textInput('label_layer_gene', 'Layer label', 'Your Guess'),
                                checkboxInput(
                                    'label_click_gene',
                                    'Enable layer-labelling by clicking on points',
                                    value = FALSE
                                ),
                                verbatimTextOutput("click_gene"),
                                actionButton(
                                    'update_layer_gene',
                                    'Label selected points (from lasso) with layer'
                                ),
                                helpText('Select points to label them with a layer guess.'),
                                helpText(
                                    'Note that only spots passing the minimum count value will be updated.'
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
                                'Gene grid (static)',
                                checkboxGroupInput(
                                    'gene_grid_samples',
                                    label = 'Select samples to show in the grid',
                                    choices = unique(sce$sample_name),
                                    selected = unique(sce$sample_name)[seq(1, 11, by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    'gene_grid_nrow',
                                    label = 'N rows',
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    'gene_grid_ncol',
                                    label = 'N columns',
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton('gene_grid_update', label = 'Update grid plot'),
                                downloadButton('downloadPlotGeneGrid', 'Download PDF'),
                                uiOutput('gene_grid_static'),
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
                                'Documentation',
                                p(
                                    'There can be a maximum of 36 unique guessed layers before we run out of colors.'
                                ),
                                p('todo')
                            )
                        )
                    )
                )
            )),
            tabPanel('layer-level data', tagList(p('TODO'))),
            tabPanel(
                'Help or feedback',
                tagList(
                    p('Please get in touch with Leonardo Collado Torres.'),
                    hr(),
                    p('The following information will be useful to them:'),
                    verbatimTextOutput('session_info')
                )
            ),
            hr(),
            p(
                'This shiny application was developed by the Data Science Team 1 at the Lieber Institute for Brain Development.'
            ),
            hr(),
            #HTML('<a href="http://www.libd.org/">'),
            img(
                src = 'http://aejaffe.com/media/LIBD_logo.jpg',
                align = 'left',
                width = '250'
            ),
            # HTML('</a>'),
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
    addResourcePath('www', system.file('app/www', package = 'spatialLIBD'))
    addResourcePath('imagedata', image_path)

    tags$head(golem::activate_js(),
        # Add here all the external resources
        # If you have a custom.css in the inst/app/www
        # Or for example, you can add shinyalert::useShinyalert() here
        #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        golem::favicon())
}
