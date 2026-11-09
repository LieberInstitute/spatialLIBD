# Build pseudobulk model boxplots and gene/test result tables with their exports.
# Share one plot object between screen and PDF, keeping the modeled grouping
# and display settings consistent with reproducible jitter.

app_layer_models_ui <- function(sce_layer, sig_genes, default_model_type) {
    tabPanel(
        "Model boxplots",
        fluidRow(
            column(
                width = 6,
                pickerInput(
                    inputId = "layer_model_test",
                    label = "Model test",
                    choices = sort(unique(sig_genes$test[
                        sig_genes$model_type ==
                            default_model_type
                    ])),
                    options = pickerOptions(
                        liveSearch = TRUE
                    )
                ),
                helpText(
                    "Short label for the statistical test done. For the model type 'enrichment', 'model test' is the selected group against all other ones. For 'pairwise' it's 'G1-G2' where the t-statistics are G1 > G2. For 'anova', it's an F-statistic."
                ),
                pickerInput(
                    inputId = "layer_geneid",
                    label = "Gene",
                    choices = sort(
                        rowData(
                            sce_layer
                        )$gene_search
                    ),
                    options = pickerOptions(
                        liveSearch = TRUE
                    )
                ),
                helpText(
                    "Select a gene to show. Only genes whose data is included are shown, so the set of genes is dependent on the 'model results' and 'model test' selected."
                ),
                selectInput(
                    inputId = "layer_boxcolor",
                    label = "Boxplot color scale",
                    choices = c(
                        "viridis",
                        "paper",
                        "bluered"
                    ),
                    selected = "viridis"
                ),
                helpText(
                    "'viridis' are color-blind friendly colors (default). 'paper' are the ones used in Maynard et al, Nature Neurosci, 2021. 'bluered' are blue and red colors."
                )
            ),
            column(
                width = 6,
                selectInput(
                    inputId = "layer_model_assayname",
                    label = "Assay name",
                    choices = assayNames(sce_layer),
                    selected = ifelse(
                        "logcounts" %in%
                            assayNames(sce_layer),
                        "logcounts",
                        assayNames(sce_layer)[1]
                    )
                ),
                helpText(
                    "Type of gene expression values you would like to see. Typical options are 'counts' for raw counts and 'logcounts' for log normalized counts."
                ),
                numericInput(
                    inputId = "layer_box_cex",
                    label = "Font size",
                    value = 2.7,
                    min = 1,
                    max = 5,
                    step = 0.1
                ),
                helpText(
                    "Select a smaller value if your group labels are too long. Default: 2.7."
                ),
                checkboxInput(
                    "layer_box_shortitle",
                    "Enable short title on boxplots.",
                    value = TRUE
                ),
                helpText(
                    "Deselect if you want a longer title that includes the gene ID (typically ENSEMBL) and the t or F statistic value."
                )
            )
        ),
        hr(),
        downloadButton(
            "layer_downloadBoxplot",
            "Download PDF"
        ),
        plotOutput("layer_boxplot", height = 600),
        div(class = "app-plot-spacing"),
        hr(),
        tags$h2(
            "Selected model and gene across all tests"
        ),
        downloadButton(
            "layer_downloadModelTable",
            "Download CSV"
        ),
        helpText(
            "This table shows the selected gene across all 'test's for the selected 'model type'. This information is useful if you want to quickly check the results for other 'test's under the same 'model type' context."
        ),
        helpText(
            "Column specifications are the following ones. 'top' is the rank of the gene ordered by the given statistic for a given 'model type' and 'test' . 'model type' is either an 'anova' F-statistic (any group is different), a 'pairwise' t-statistic (group 1 > group 2), or an 'enrichment' t-statistic (selected group > all others). 'test' is the short name identifying the selected group. 'gene' is the gene symbol. 'stat' is the F or t-statistic depending on the 'model type'. 'pval' is the un-adjusted p-value and 'fdr' is the FDR-adjusted p-value. 'gene_index' is the row number for from the input SpatialExperiment object corresponding to this gene. 'ensembl' is the gene ENSEMBL ID."
        ),
        DT::DTOutput("layer_model_table"),
        hr(),
        tags$h2(
            "Selected model and test across all genes"
        ),
        downloadButton(
            "layer_downloadModelTable_full",
            "Download CSV"
        ),
        helpText(
            "This table shows the selected 'model type' and 'test' across all genes. This table is useful if you want to find how other genes rank under the same model type and test context. It can be useful to find genes to select."
        ),
        DT::DTOutput("layer_model_table_full"),
    )
}

app_layer_models_server <- function(input, output, sce, sig_genes, group_var) {
    selected_row <- reactive({
        which(sig_genes$gene_index == match(input$layer_geneid, rowData(sce)$gene_search) &
            sig_genes$test == input$layer_model_test & sig_genes$model_type == input$layer_model)
    })
    # Cache one plot object for both display and download. Jitter has its own
    # reproducible seed, so neither path needs to modify the session's RNG state.
    plot <- reactive({
        i <- selected_row()
        req(length(i) == 1L)
        do.call(layer_boxplot, c(
            list(
                i = i, sig_genes = sig_genes, sce_layer = sce,
                short_title = input$layer_box_shortitle, cex = input$layer_box_cex,
                group_var = group_var, assayname = input$layer_model_assayname
            ),
            app_boxplot_colors(input$layer_boxcolor)
        ))
    })
    table <- sig_genes[, seq_len(match("ensembl", colnames(sig_genes)))]
    gene_table <- reactive(as.data.frame(table[
        table$model_type == input$layer_model & table$ensembl == sub(".*; ", "", input$layer_geneid),
    ]))
    test_table <- reactive(as.data.frame(table[
        table$model_type == input$layer_model & table$test == input$layer_model_test,
    ]))
    stem <- function() paste("layer", input$layer_model, input$layer_model_test, sep = "_")
    output$layer_boxplot <- renderPlot(print(plot()), width = 600, height = "auto")
    output$layer_downloadBoxplot <- app_pdf_download(
        function() {
            paste("layer_boxplot", input$layer_model, input$layer_model_test,
                input$layer_geneid,
                sep = "_"
            )
        }, function() print(plot())
    )
    output$layer_model_table <- app_table(gene_table, direction = "asc")
    output$layer_model_table_full <- app_table(test_table, direction = "asc")
    output$layer_downloadModelTable <- app_csv_download(
        function() paste(stem(), input$layer_geneid, sep = "_"), gene_table
    )
    output$layer_downloadModelTable_full <- app_csv_download(stem, test_table)
    list(plot = plot, gene_table = gene_table, test_table = test_table)
}
