app_spot_controls_ui <- function(
  spe, spe_discrete_vars, spe_continuous_vars,
  default_cluster, auto_crop_default, datatype
) {
    red_dim_names <- sort(reducedDimNames(spe))
    sidebarPanel(
        pickerInput(
            inputId = "sample",
            label = "Sample to plot",
            choices = unique(spe$sample_id),
            options = pickerOptions(liveSearch = TRUE)
        ),
        helpText(
            "The sample ID to visualize in most tabs except the 'grid' ones."
        ),
        hr(),
        selectInput(
            inputId = "imageid",
            label = "Image name",
            choices = c(
                if (datatype == "Xenium") {
                    NULL
                } else {
                    "edited_imaged"
                },
                unique(imgData(spe)$image_id)
            ),
            selected = unique(imgData(spe)$image_id)[1]
        ),
        helpText(
            "The name of the background image you want to visualize."
        ),
        hr(),
        pickerInput(
            inputId = "cluster",
            label = "Discrete variable to plot",
            choices = spe_discrete_vars,
            selected = default_cluster,
            options = pickerOptions(liveSearch = TRUE)
        ),
        helpText(
            "Typically cluster labels or any other discrete variable."
        ),
        hr(),
        pickerInput(
            inputId = "clustercolor",
            label = "Discrete color scale",
            choices = c(
                colnames(colData(spe))[grep(
                    "_colors$",
                    colnames(colData(spe))
                )],
                with(
                    subset(
                        paletteer::palettes_d_names,
                        length >=
                            length(unique(colData(spe)[[
                                default_cluster
                            ]]))
                    ),
                    paste0(package, "::", palette)
                )
            ),
            selected = ifelse(
                paste0(default_cluster, "_colors") %in%
                    colnames(colData(spe)),
                paste0(default_cluster, "_colors"),
                "Polychrome::palette36"
            ),
            options = pickerOptions(liveSearch = TRUE)
        ),
        helpText(
            "Either any columns that end with '_colors' or options from the 'paletteer' R package. Only valid options are shown."
        ),
        HTML(
            "You can visually explore these options at the <a href='https://emilhvitfeldt.github.io/r-color-palettes/discrete.html'>Palette Picker</a> website."
        ),
        checkboxInput(
            "clustercolor_direction",
            "Should the color order be reversed?",
            value = FALSE
        ),
        hr(),
        pickerInput(
            inputId = "geneid",
            label = "Continuous variable(s) to plot",
            choices = c(
                spe_continuous_vars,
                sort(rowData(spe)$gene_search)
            ),
            options = pickerOptions(liveSearch = TRUE),
            multiple = TRUE,
            selected = c(
                spe_continuous_vars,
                sort(rowData(spe)$gene_search)
            )[1]
        ),
        helpText(
            "Typically gene(s) or any other continuous variable(s)."
        ),
        hr(),
        selectInput(
            inputId = "multi_gene_method",
            label = "Multi-gene method",
            choices = c("z_score", "pca", "sparsity"),
            selected = "z_score"
        ),
        helpText(
            "When applicable, the method used to combine multiple continuous variables."
        ),
        hr(),
        selectInput(
            inputId = "assayname",
            label = "Gene scale",
            choices = assayNames(spe),
            selected = assayNames(spe)[length(assayNames(
                spe
            ))]
        ),
        helpText(
            "The name of the gene values you want to visualize."
        ),
        hr(),
        numericInput(
            inputId = "minCount",
            label = "Minimum count value",
            value = 0,
            min = -1,
            step = 1
        ),
        helpText(
            "You can manually enter any number then press enter in your keyboard. This is useful for extreme values."
        ),
        hr(),
        numericInput(
            inputId = "cap_percentile",
            label = "Maximum count percentile",
            value = 1,
            min = 0.05,
            max = 1,
            step = 0.05
        ),
        helpText(
            "When set below 1, counts above the given percentile (e.g. 0.95 for 95th) are capped, typically to control outliers"
        ),
        hr(),
        selectInput(
            inputId = "genecolor",
            label = "Gene color scale",
            choices = c(
                "viridis",
                "paper",
                "magma",
                "inferno",
                "plasma",
                "cividis",
                "rocket",
                "mako",
                "turbo"
            ),
            selected = "viridis"
        ),
        helpText(
            "The viridis scale is color-blind friendly."
        ),
        checkboxInput(
            "genecolor_direction",
            "Should the colors be ordered from darkest to lightest?",
            value = TRUE
        ),
        hr(),
        numericInput(
            "alphalevel",
            "Spot transparency level",
            value = NA,
            min = NA,
            max = 1,
            step = 0.1
        ),
        helpText(
            "Use values between 0 (transparent) to 1 (full color). Leave empty to show spots below the minimum value threshold in a transparent color."
        ),
        hr(),
        numericInput(
            "pointsize",
            "Spot point size",
            value = 2,
            min = 1,
            max = 3,
            step = 0.1
        ),
        helpText(
            "We recommend that you use values between 1.25 and 2 (default)."
        ),
        hr(),
        numericInput(
            "guidepointsize",
            "Guide point size",
            value = 2,
            min = 1,
            max = 3,
            step = 0.1
        ),
        helpText(
            "This controls the point size in the legend when visualizing discrete variables."
        ),
        hr(),
        checkboxInput(
            "auto_crop",
            "Should the image be automatically cropped?",
            value = auto_crop_default
        ),
        hr(),
        selectInput(
            inputId = "reduced_name",
            label = "Reduced dimensions",
            choices = red_dim_names,
            selected = red_dim_names[length(red_dim_names)]
        ),
        helpText(
            "The first two dimensions are shown in the 'clusters (interactive)' tab."
        ),
        hr(),
        checkboxInput(
            "dropNA",
            "Drop NA layer entries in the CSV file?",
            value = TRUE
        ),
        downloadButton(
            "downloadData",
            "Download manual annotations"
        ),
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
    )
}

# spe() includes current manual annotations; palette choices update with labels.
app_spot_controls_server <- function(input, session, spe) {
    previous_variable <- reactiveVal(NULL)
    observeEvent(
        {
            req(input$cluster)
            list(input$cluster, spe()[[input$cluster]])
        },
        {
            req(input$cluster)
            palette <- app_palette_choices(spe(), input$cluster)
            current <- isolate(input$clustercolor)
            selected <- if (length(current) == 1 && current %in% palette$choices) current else palette$selected
            # A supplied palette for a newly selected variable takes precedence.
            changed <- !identical(input$cluster, previous_variable())
            if (changed && paste0(input$cluster, "_colors") %in% palette$choices) selected <- palette$selected
            previous_variable(input$cluster)
            updatePickerInput(session, "clustercolor", choices = palette$choices, selected = selected)
        }
    )
    discrete <- reactive({
        req(input$cluster, input$clustercolor)
        app_discrete_colors(
            spe(), input$cluster, input$clustercolor,
            isTRUE(input$clustercolor_direction)
        )
    })
    continuous <- reactive({
        req(input$genecolor)
        if (input$genecolor == "paper") {
            colors <- c("aquamarine4", "springgreen", "goldenrod", "red")
            if (isTRUE(input$genecolor_direction)) colors else rev(colors)
        } else {
            viridisLite::viridis(21,
                option = input$genecolor,
                direction = if (isTRUE(input$genecolor_direction)) 1 else -1
            )
        }
    })
    list(discrete = discrete, continuous = continuous)
}
