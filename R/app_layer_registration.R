# Build the spatial-registration view for example or uploaded query statistics.
# Match query genes to the reference model results, compute correlations and
# cluster annotations, and share them across the heatmap, table, and exports.

app_layer_registration_ui <- function() {
    tabPanel(
        "Spatial registration",
        fluidRow(
            column(
                width = 6,
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
                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv">this example file</a>, documented <a href="https://research.libd.org/spatialLIBD/reference/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.html">here</a>. For more context, check <a href="https://www.nature.com/articles/s41593-020-00787-0#Fig5">this figure</a>.'
                    )
                )
            ),
            column(
                width = 6,
                numericInput(
                    "layer_tstat_max",
                    label = "Maximum correlation",
                    value = 0.81,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                helpText(
                    "Use a smaller positive number to change the range of the color scale used. Use 1 if you want the color range to reflect the maximum range of correlation values. Default: 0.81."
                ),
                numericInput(
                    "layer_confidence_threshold",
                    label = "Annotation confidence threshold",
                    value = 0.25,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                helpText(
                    "Minimum correlation for a high confidence annotation. Higher values are more strict in annotating. Default: 0.25."
                ),
                numericInput(
                    "layer_cutoff_merge_ratio",
                    label = "Annotation cutoff merge ratio",
                    value = 0.25,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                helpText(
                    "Merging threshold for the ratio of the difference between the current one to the next closest correlation, relative to the next closest correlation: (current - next_cor) / next_cor. Lower values are more strict in annotating. Default: 0.25."
                )
            )
        ),
        hr(),
        downloadButton(
            "layer_downloadTstatCor",
            "Download PDF"
        ),
        plotOutput("layer_tstat_cor_plot", height = 600),
        div(class = "app-plot-spacing"),
        hr(),
        downloadButton(
            "layer_downloadTstatCorTable",
            "Download CSV"
        ),
        helpText(
            "Correlation matrix that is visually illustrated with the previous plot."
        ),
        downloadButton(
            "layer_downloadTstatCor_annotation",
            "Download CSV"
        ),
        helpText("Annotated clusters."),
        DT::DTOutput("layer_tstat_cor_table")
    )
}

app_layer_registration_server <- function(input, output, sce, models, group_var) {
    correlations <- reactive({
        req(input$layer_model)
        query <- if (is.null(input$externalTstat)) {
            spatialLIBD::tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer
        } else {
            utils::read.csv(input$externalTstat$datapath,
                stringsAsFactors = FALSE,
                na.strings = "", check.names = FALSE, row.names = 1
            )
        }
        genes <- if ("ensembl" %in% colnames(query)) query$ensembl else rownames(query)
        validate(need(
            length(intersect(genes, models[[input$layer_model]]$ensembl)) >= 2,
            "At least two matching genes are needed for spatial registration."
        ))
        layer_stat_cor(query, models, input$layer_model)
    })
    annotations <- reactive(annotate_registered_clusters(
        correlations(),
        input$layer_confidence_threshold, input$layer_cutoff_merge_ratio
    ))
    plot <- reactive({
        matrix <- correlations()
        max_cor <- max(0.1, input$layer_tstat_max)
        layer_stat_cor_plot(matrix, max_cor, -max_cor,
            cluster_rows = FALSE,
            cluster_columns = FALSE, annotation = annotations(),
            query_colors = get_colors(clusters = rownames(matrix)),
            reference_colors = app_model_colors(sce, group_var, colnames(matrix), input$layer_model)
        )
    })
    stem <- function() paste0("layer_TstatCor_", input$layer_model)
    output$layer_tstat_cor_plot <- renderPlot(print(plot()), width = 800, height = "auto")
    output$layer_tstat_cor_table <- app_table(correlations, row_names = TRUE)
    output$layer_downloadTstatCor <- app_pdf_download(
        function() paste0("layer_TstatCorHeatmap_", input$layer_model), function() print(plot()),
        width = function() 12
    )
    output$layer_downloadTstatCorTable <- app_csv_download(stem, correlations, row_names = TRUE)
    output$layer_downloadTstatCor_annotation <- app_csv_download(
        function() paste0("layer_TstatCor_annotated_clusters_", input$layer_model),
        annotations,
        row_names = TRUE
    )
    list(correlations = correlations, annotations = annotations, plot = plot)
}
