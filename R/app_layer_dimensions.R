app_layer_dimensions_ui <- function(sce_layer, default_reduced_dim, default_cluster) {
    tabPanel(
        "Reduced Dim",
        fluidRow(
            column(
                width = 6,
                selectInput(
                    inputId = "layer_which_dim",
                    label = "Reduced Dimension",
                    choices = sort(reducedDimNames(
                        sce_layer
                    )),
                    selected = default_reduced_dim
                ),
                helpText(
                    "Select a reduced dimension name to visualize. These are pre-computed with the pseudo-bulk level data (layer-level for Maynard et al, Nature Neurosci, 2021)."
                ),
                numericInput(
                    inputId = "layer_reduced_dim_ncomponents",
                    label = "Number of dimensions to show",
                    value = 2,
                    min = 2,
                    max = if (nzchar(default_reduced_dim)) {
                        ncol(reducedDim(sce_layer, default_reduced_dim))
                    } else {
                        2
                    },
                    step = 1
                ),
                helpText(
                    "Use a minimum value of 2. If you use the up arrow button, it is automatically restricted to the maximum range for the selected reduced dimension name. For values of 3 or larger, paired scatterplots will be shown."
                )
            ),
            column(
                width = 6,
                selectInput(
                    inputId = "layer_which_dim_color",
                    label = "Color by",
                    choices = sort(
                        colnames(colData(
                            sce_layer
                        ))[
                            !grepl(
                                "_colors$",
                                colnames(colData(
                                    sce_layer
                                ))
                            )
                        ]
                    ),
                    selected = default_cluster
                ),
                helpText(
                    "Select a variable name to use for coloring the pseudo-bulked samples in the reduced dimensions plot."
                ),
                numericInput(
                    inputId = "layer_reduced_dim_theme_size",
                    label = "Theme font size: default 30.",
                    value = 30,
                    min = 1,
                    step = 1
                ),
                helpText(
                    "This controls the text font size. For example, if you are visualizing 10 dimensions, consider using a theme size of 13."
                ),
                numericInput(
                    inputId = "layer_reduced_dim_point_size",
                    label = "Point size: default 7.",
                    value = 7,
                    min = 1,
                    step = 1
                ),
                helpText(
                    "This controls the point size. For example, if you are visualizing 10 dimensions, consider using a point size of 4."
                )
            )
        ),
        downloadButton(
            "layer_downloadReducedDim",
            "Download PDF"
        ),
        plotOutput(
            "layer_reduced_dim",
            width = "auto",
            height = "auto"
        ),
        div(class = "app-plot-spacing"),
    )
}

app_layer_dimensions_server <- function(input, output, session, sce) {
    observeEvent(input$layer_which_dim, {
        req(input$layer_which_dim %in% reducedDimNames(sce))
        updateNumericInput(session, "layer_reduced_dim_ncomponents",
            value = 2,
            max = ncol(reducedDim(sce, input$layer_which_dim))
        )
    })
    plot <- reactive({
        validate(need(input$layer_which_dim %in% reducedDimNames(sce), "No reduced dimensions available."))
        req(input$layer_which_dim_color, input$layer_reduced_dim_ncomponents)
        p <- scater::plotReducedDim(sce,
            dimred = input$layer_which_dim,
            colour_by = input$layer_which_dim_color, theme_size = input$layer_reduced_dim_theme_size,
            point_size = input$layer_reduced_dim_point_size, ncomponents = input$layer_reduced_dim_ncomponents,
            label_format = c("%s %02i", " (%i%%)")
        )
        variable <- input$layer_which_dim_color
        # Preserve the original layer palettes without changing global ggplot themes.
        colors <- if (variable %in% c("layer_guess", "layer_guess_reordered")) {
            app_paper_colors()
        } else if (variable == "layer_guess_reordered_short") {
            app_paper_colors(TRUE)
        } else {
            sce[[paste0(variable, "_colors")]]
        }
        if (!is.null(colors)) {
            p <- p + scale_color_manual(
                values = colors,
                name = if (startsWith(variable, "layer_guess")) "Layer" else variable,
                labels = levels(sce[[variable]]), breaks = levels(sce[[variable]])
            )
        }
        p
    })
    output$layer_reduced_dim <- renderPlot(print(plot()),
        width = function() 600 + 100 * input$layer_reduced_dim_ncomponents %/% 2,
        height = function() 500 + 100 * input$layer_reduced_dim_ncomponents %/% 2
    )
    output$layer_downloadReducedDim <- app_pdf_download(
        function() paste0("layer_reducedDim_", input$layer_which_dim), function() print(plot()),
        width = function() 10 + input$layer_reduced_dim_ncomponents %/% 2,
        height = function() 9 + input$layer_reduced_dim_ncomponents %/% 2
    )
    list(plot = plot)
}
