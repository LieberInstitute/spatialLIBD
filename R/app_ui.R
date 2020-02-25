#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @import SingleCellExperiment
#' @importFrom DT DTOutput
#' @importFrom SummarizedExperiment assays
app_ui <- function() {
    ## Get options
    sce <- golem::get_golem_options('sce')
    sce_layer <- golem::get_golem_options('sce_layer')
    image_path <- golem::get_golem_options('image_path')
    sig_genes <- golem::get_golem_options('sig_genes')
    spatial_libd_var <- golem::get_golem_options('spatial_libd_var')

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
                                'spatialLIBD',
                                golem::get_golem_options('sce_discrete_vars')
                            )
                        ),
                        selectInput(
                            inputId = 'reduced_name',
                            label = 'Reduced dimensions',
                            choices = sort(reducedDimNames(sce)),
                            selected = reducedDimNames(sce)[length(reducedDimNames(sce))]
                        ),
                        pickerInput(
                            inputId = 'geneid',
                            label = 'Gene (or count variable)',
                            choices = c(
                                golem::get_golem_options('sce_continuous_vars'),
                                sort(rowData(sce)$gene_search)
                            ),
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
                            tabPanel(
                                'Raw summary',
                                tags$br(),
                                HTML(
                                    'Basic information overview about the spot-level SingleCellExperiment object. You can download it using <code>spatialLIBD::fetch_data(type = "sce")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput('raw_summary'),
                                helpText(
                                    'When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it. For more information check the Documentation tab.'
                                )
                            ),
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
                                    selected = unique(sce$sample_name)[seq(1, length(unique(sce$sample_name)), by = 4)],
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
                            tabPanel('Documentation',
                                includeMarkdown(
                                    file.path(resourcePaths()['www'], 'documentation_sce.md')
                                ))
                        )
                    )
                )
            )),
            tabPanel('layer-level data', tagList(
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = 'layer_model',
                            label = 'Model results',
                            choices = c('specificity', 'pairwise', 'anova'),
                            selected = 'specificity'
                        ),
                        hr(),
                        pickerInput(
                            inputId = 'layer_geneid',
                            label = 'Gene',
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
                                'Raw summary',
                                tags$br(),
                                HTML(
                                    'Basic information overview about the layer-level SingleCellExperiment object (pseudo-bulked from the spot-level data). You can download it using <code>spatialLIBD::fetch_data(type = "sce_layer")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput('layer_raw_summary'),
                                helpText(
                                    'When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it. For more information check the Documentation tab.'
                                )
                            ),
                            tabPanel(
                                'Reduced Dim',
                                selectInput(
                                    inputId = 'layer_which_dim',
                                    label = 'Reduced Dimension',
                                    choices = sort(reducedDimNames(sce_layer)),
                                    selected = 'PCA'
                                ),
                                selectInput(
                                    inputId = 'layer_which_dim_color',
                                    label = 'Color by',
                                    choices = sort(colnames(colData(sce_layer))),
                                    selected = spatial_libd_var
                                ),
                                downloadButton('layer_downloadReducedDim', 'Download PDF'),
                                plotOutput('layer_reduced_dim'),
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
                                'Model boxplots',
                                selectInput(
                                    inputId = 'layer_model_test',
                                    label = 'Model test',
                                    choices = sort(unique(sig_genes$test[sig_genes$model_type == 'specificity']))
                                ),
                                selectInput(
                                    inputId = 'layer_boxcolor',
                                    label = 'Boxplot color scale',
                                    choices = c('viridis', 'paper'),
                                    selected = 'viridis'
                                ),
                                checkboxInput(
                                    'layer_box_shortitle',
                                    'Enable short title on boxplots.',
                                    value = TRUE
                                ),
                                hr(),
                                downloadButton('layer_downloadBoxplot', 'Download PDF'),
                                plotOutput('layer_boxplot'),
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
                                downloadButton('layer_downloadModelTable', 'Download CSV'),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput('layer_model_table')
                            ),
                            tabPanel(
                                'Gene Set Enrichment',
                                fileInput(
                                    'geneSet',
                                    'Upload a CSV file with one column per gene set with a header row and then Ensembl gene IDs as values.',
                                    accept = c(
                                        'text/csv',
                                        '.csv',
                                        'text/comma-separated-values,text/plain'
                                    )
                                ),
                                helpText(
                                    'It should be a CSV file without row names and similar to ',
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/asd_sfari_geneList.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    'layer_gene_fdrcut',
                                    label = 'FDR cutoff',
                                    value = 0.1,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                hr(),
                                downloadButton('layer_downloadGeneSet', 'Download PDF'),
                                plotOutput('layer_gene_set_plot'),
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
                                downloadButton('layer_downloadGeneSetTable', 'Download CSV'),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput('layer_gene_set_table')
                            ),
                            tabPanel(
                                'stat correlation',
                                fileInput(
                                    'externalTstat',
                                    'Upload a CSV file with one column per cell type or layer that contains the specificity t-stat equivalent and with Ensembl gene IDs as the row names.',
                                    accept = c(
                                        'text/csv',
                                        '.csv',
                                        'text/comma-separated-values,text/plain'
                                    )
                                ),
                                helpText(
                                    'It should be a CSV file similar to ',
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    'layer_tstat_max',
                                    label = 'Maximum correlation',
                                    value = 0.81,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                hr(),
                                downloadButton('layer_downloadTstatCor', 'Download PDF'),
                                plotOutput('layer_tstat_cor_plot'),
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
                                downloadButton('layer_downloadTstatCorTable', 'Download CSV'),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput('layer_tstat_cor_table')
                            ),
                            tabPanel('Documentation',
                                includeMarkdown(
                                    file.path(resourcePaths()['www'], 'documentation_sce_layer.md')
                                ))
                        )
                    )
                )
            )),
            tabPanel(
                'Help or feedback',
                tagList(
                    p('Please get in touch with the spatialLIBD authors through the ', HTML('<a href="https://support.bioconductor.org/">Bioconductor Support Website</a> (using the spatialLIBD tag) or through <a href="https://github.com/LieberInstitute/spatialLIBD/issues">GitHub</a>. Remember to help others help you by including all the information required to reproduce the problem you noticed. Thank you!')),
                    hr(),
                    p('The following information will be useful to them:'),
                    verbatimTextOutput('session_info')
                )
            ),
            hr(),
            p(
                'This shiny application was developed by the Data Science Team I at the Lieber Institute for Brain Development.'
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
    addResourcePath('www',
        system.file('app', 'www', package = 'spatialLIBD'))
    addResourcePath('imagedata', image_path)

    tags$head(golem::activate_js(),
        # Add here all the external resources
        # If you have a custom.css in the inst/app/www
        # Or for example, you can add shinyalert::useShinyalert() here
        #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        golem::favicon())
}
