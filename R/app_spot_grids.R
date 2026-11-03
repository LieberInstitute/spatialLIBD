app_cluster_grid_ui <- function(spe) {
    tabPanel(
        "Clusters grid (static)",
        checkboxInput(
            "grid_spatial_clus",
            "Use spatial image?",
            value = FALSE
        ),
        checkboxGroupInput(
            "grid_samples",
            label = "Select samples to show in the grid",
            choices = unique(spe$sample_id),
            selected = unique(spe$sample_id)[seq(
                1,
                length(unique(spe$sample_id)),
                by = 4
            )],
            inline = TRUE
        ),
        numericInput(
            "grid_nrow",
            label = "N rows",
            value = round(
                length(unique(spe$sample_id)) /
                    (3 * 4) +
                    0.5
            ),
            min = 1
        ),
        numericInput(
            "grid_ncol",
            label = "N columns",
            value = 3,
            min = 1
        ),
        actionButton(
            "grid_update",
            label = "Update grid plot"
        ),
        downloadButton(
            "downloadPlotHistologyGrid",
            "Download PDF"
        ),
        uiOutput("grid_static"),
        helpText(
            "Click the 'Update grid plot' button above to re-make this plot."
        ),
        div(class = "app-plot-spacing"),
    )
}

app_gene_grid_ui <- function(spe) {
    tabPanel(
        "Gene grid (static)",
        checkboxInput(
            "grid_spatial_gene",
            "Use spatial image?",
            value = FALSE
        ),
        checkboxGroupInput(
            "gene_grid_samples",
            label = "Select samples to show in the grid",
            choices = unique(spe$sample_id),
            selected = unique(spe$sample_id)[seq(
                1,
                length(unique(spe$sample_id)),
                by = 4
            )],
            inline = TRUE
        ),
        numericInput(
            "gene_grid_nrow",
            label = "N rows",
            value = round(
                length(unique(spe$sample_id)) /
                    (3 * 4) +
                    0.5
            ),
            min = 1
        ),
        numericInput(
            "gene_grid_ncol",
            label = "N columns",
            value = 3,
            min = 1
        ),
        actionButton(
            "gene_grid_update",
            label = "Update grid plot"
        ),
        downloadButton(
            "downloadPlotGeneGrid",
            "Download PDF"
        ),
        uiOutput("gene_grid_static"),
        textOutput("gene_grid_warnings"),
        helpText(
            "Click the 'Update grid plot' button above to re-make this plot."
        ),
        div(class = "app-plot-spacing"),
    )
}

# Each grid stores the plot AND its layout/labels at the Update event. Downloads
# therefore describe the displayed grid even when controls have since changed.
app_spot_grids_server <- function(input, output, spe, colors, settings) {
    clusters <- eventReactive(input$grid_update,
        {
            req(length(input$grid_samples) > 0)
            plots <- do.call(vis_grid_clus, c(list(
                spe = spe(), clustervar = input$cluster, colors = colors$discrete(),
                guide_point_size = input$guidepointsize, sort_clust = FALSE,
                return_plots = TRUE, spatial = isTRUE(input$grid_spatial_clus),
                sample_order = input$grid_samples, ... = paste(" with", input$cluster)
            ), settings()))
            list(
                p = cowplot::plot_grid(
                    plotlist = plots, nrow = input$grid_nrow,
                    ncol = input$grid_ncol
                ),
                rows = input$grid_nrow, cols = input$grid_ncol,
                stem = paste("static_cluster_grid", input$cluster,
                    paste(input$grid_samples, collapse = "_"),
                    sep = "_"
                )
            )
        },
        ignoreNULL = FALSE
    )
    genes <- eventReactive(input$gene_grid_update,
        {
            req(length(input$gene_grid_samples) > 0)
            result <- app_capture_warnings({
                plots <- do.call(vis_grid_gene, c(list(
                    spe = spe(), return_plots = TRUE, spatial = isTRUE(input$grid_spatial_gene),
                    sample_order = input$gene_grid_samples
                ), settings(), app_gene_settings(input, colors$continuous)))
                cowplot::plot_grid(
                    plotlist = plots, nrow = input$gene_grid_nrow,
                    ncol = input$gene_grid_ncol
                )
            })
            c(result, list(
                rows = input$gene_grid_nrow, cols = input$gene_grid_ncol,
                stem = paste("static_gene_grid", paste(input$geneid, collapse = "_"),
                    paste(input$gene_grid_samples, collapse = "_"),
                    sep = "_"
                )
            ))
        },
        ignoreNULL = FALSE
    )

    output$grid_static <- renderUI(plotOutput("histology_grid",
        width = 600 * clusters()$cols, height = 600 * clusters()$rows
    ))
    output$histology_grid <- renderPlot(print(clusters()$p), width = "auto", height = "auto")
    output$gene_grid_static <- renderUI(plotOutput("gene_grid",
        width = 600 * genes()$cols, height = 600 * genes()$rows
    ))
    output$gene_grid <- renderPlot(print(genes()$p), width = "auto", height = "auto")
    output$gene_grid_warnings <- renderText(app_warning_text(genes))
    output$downloadPlotHistologyGrid <- app_pdf_download(
        function() clusters()$stem, function() print(clusters()$p),
        width = function() 9 * clusters()$cols, height = function() 8 * clusters()$rows
    )
    output$downloadPlotGeneGrid <- app_pdf_download(
        function() genes()$stem, function() print(genes()$p),
        width = function() 8 * genes()$cols, height = function() 8 * genes()$rows
    )
    list(clusters = clusters, genes = genes)
}
