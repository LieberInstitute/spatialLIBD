app_spot_documentation_ui <- function(docs_path) {
    tabPanel(
        "Documentation",
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

app_cluster_plot_ui <- function() {
    tabPanel(
        "Clusters (static)",
        checkboxInput(
            "side_by_side_histology",
            "Show clusters and background image side by side.",
            value = FALSE,
            width = "100%"
        ),
        downloadButton(
            "downloadPlotHistology",
            "Download PDF"
        ),
        plotOutput("histology"),
        div(class = "app-plot-spacing"),
    )
}

app_gene_plot_ui <- function() {
    tabPanel(
        "Gene (static)",
        checkboxInput(
            "side_by_side_gene",
            "Show gene and background image side by side.",
            value = FALSE,
            width = "100%"
        ),
        downloadButton(
            "downloadPlotGene",
            "Download PDF"
        ),
        plotOutput("gene"),
        div(class = "app-plot-spacing"),
        textOutput("gene_warnings")
    )
}

app_spot_plots_server <- function(input, output, spe, colors, settings) {
    histology <- reactive({
        req(input$sample, input$cluster)
        plot <- do.call(vis_clus, c(list(
            spe = spe(), sampleid = input$sample, clustervar = input$cluster,
            colors = colors$discrete(), guide_point_size = input$guidepointsize,
            ... = paste(" with", input$cluster)
        ), settings()))
        app_side_by_side(plot, isTRUE(input$side_by_side_histology), settings()$datatype)
    })
    gene <- reactive({
        req(input$sample)
        app_capture_warnings({
            plot <- do.call(vis_gene, c(
                list(spe = spe(), sampleid = input$sample),
                settings(), app_gene_settings(input, colors$continuous)
            ))
            app_side_by_side(plot, isTRUE(input$side_by_side_gene), settings()$datatype)
        })
    })
    output$raw_summary <- renderPrint(print(spe()), width = 80)
    output$histology <- renderPlot(histology(),
        height = 600,
        width = function() 600 * if (isTRUE(input$side_by_side_histology)) 2 else 1
    )
    output$gene <- renderPlot(gene()$p,
        height = 600,
        width = function() 600 * if (isTRUE(input$side_by_side_gene)) 2 else 1
    )
    output$gene_warnings <- renderText(app_warning_text(gene))
    output$downloadPlotHistology <- app_pdf_download(
        function() paste("static_cluster", input$cluster, input$sample, sep = "_"),
        function() print(histology()),
        height = function() 8,
        width = function() 9 * if (isTRUE(input$side_by_side_histology)) 2 else 1
    )
    output$downloadPlotGene <- app_pdf_download(
        function() paste("static_gene", paste(input$geneid, collapse = "_"), input$sample, sep = "_"),
        function() print(gene()$p),
        width = function() 8 * if (isTRUE(input$side_by_side_gene)) 2 else 1
    )
    list(histology = histology, gene = gene)
}
