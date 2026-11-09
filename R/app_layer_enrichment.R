# Build the gene-set enrichment view using uploaded sets or the example sets.
# Connect model, direction, and FDR controls to shared enrichment results, then
# present those results as a heatmap, table, and PDF/CSV downloads.

app_layer_enrichment_ui <- function() {
    tabPanel(
        "Gene Set Enrichment",
        fluidRow(
            column(
                width = 6,
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
                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/asd_sfari_geneList.csv">this example file</a>. For more context, check <a href="https://www.nature.com/articles/s41593-020-00787-0#Fig6">this figure</a>.'
                    )
                )
            ),
            column(
                width = 6,
                numericInput(
                    "layer_gene_fdrcut",
                    label = "FDR cutoff",
                    value = 0.1,
                    min = 0,
                    max = 1,
                    step = 0.01
                ),
                helpText(
                    "FDR cutoff for the gene enrichment odd ratios. Odd ratios with a p-value below the FDR cutoff won't be colored in the resulting heatmap. Use 1 to show all. Default: 0.1."
                ),
                checkboxInput(
                    "enrichment_reverse",
                    "Reverse enrichment direction. Only applies for 'model results' of type 'enrichment' or 'pairwise'.",
                    value = FALSE
                ),
                helpText(
                    "For 'enrichment', switch to 'depletion'. For 'pairwise', reverse the direction."
                )
            )
        ),
        hr(),
        downloadButton(
            "layer_downloadGeneSet",
            "Download PDF"
        ),
        plotOutput("layer_gene_set_plot"),
        div(class = "app-plot-spacing"),
        hr(),
        downloadButton(
            "layer_downloadGeneSetTable",
            "Download CSV"
        ),
        helpText(
            "Column specifications are the following ones. 'OR' is the odds ratio for enrichment among the input genes for the given 'ID' set and the genes significantly associated with given 'test' and 'model type' combination. 'Pval' is the odds ratio un-adjusted p-value. ''model type' is either an 'anova' F-statistic (any group is different), a 'pairwise' t-statistic (group 1 > group 2), or an 'enrichment' t-statistic (selected group > all others). 'test' is the short name identifying the selected group. 'ID' is the user-chosen label for identifying a given gene set: the example data has 3 such sets. 'fdr_cut' is the FDR cutoff on the odd ratio p-values colored in the previous heatmap."
        ),
        DT::DTOutput("layer_gene_set_table")
    )
}

app_layer_enrichment_server <- function(input, output, sce, models, group_var) {
    result <- reactive({
        req(input$layer_model)
        gene_set_enrichment(app_read_gene_sets(input$geneSet), input$layer_gene_fdrcut,
            models, input$layer_model,
            reverse = input$layer_model != "anova" && isTRUE(input$enrichment_reverse)
        )
    })
    plot <- reactive({
        enriched <- result()
        gene_set_enrichment_plot(enriched,
            plot_SetSize_bar = TRUE,
            model_colors = app_model_colors(sce, group_var, unique(enriched$test))
        )
    })
    stem <- function() {
        paste0(
            "layer_GeneSetEnrichment_", input$layer_model,
            "_fdrcut", input$layer_gene_fdrcut
        )
    }
    output$layer_gene_set_plot <- renderPlot(print(plot()), width = 600, height = 600)
    output$layer_gene_set_table <- app_table(result, direction = "desc")
    output$layer_downloadGeneSet <- app_pdf_download(stem, function() print(plot()))
    output$layer_downloadGeneSetTable <- app_csv_download(stem, result)
    list(result = result, plot = plot)
}
