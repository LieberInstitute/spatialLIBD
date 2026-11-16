# Shared selections; filters operate on one eligible set for all four panels.
app_spot_controls_ui <- function(spe, spe_discrete_vars, spe_continuous_vars,
    default_cluster, auto_crop_default, datatype) {
    catalog <- app_image_catalog(spe)
    samples <- unique(spe$sample_id)
    images <- app_image_choices(catalog)
    dimensions <- sort(reducedDimNames(spe))
    features <- if (length(spe_continuous_vars)) spe_continuous_vars[1] else sort(rowData(spe)$gene_search)[1]
    palette <- app_palette_choices(spe, default_cluster)
    cluster_labels <- as.character(spe[[default_cluster]])
    cluster_labels[is.na(cluster_labels)] <- "(missing)"
    cluster_labels <- sort(unique(cluster_labels))
    sidebarPanel(width = 3,
        shinyWidgets::pickerInput("sample", app_control_label("sample", "Sample",
            "The sample ID to visualize in Interactive and preview in Edit image. Static grid has its own sample selection."), choices = samples,
            options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        selectizeInput("imageid", app_control_label("imageid", "Histology image",
            "Choose the background image to visualize, or None to hide it. Full-resolution pyramids reveal more detail as you zoom. Select edited_imaged to display your low-resolution edits; this keeps the edited image at every zoom level."), choices = c("None" = "", images),
            selected = if (length(catalog[[samples[1]]]$default)) catalog[[samples[1]]]$default else "",
            options = list(allowEmptyOption = TRUE)),
        shinyWidgets::pickerInput("cluster", app_control_label("cluster", "Discrete variable",
            "Typically cluster labels or any other discrete variable. Colors show its categories in the spatial and embedding discrete panels, or the discrete static grid."), choices = spe_discrete_vars,
            selected = default_cluster, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        selectizeInput("geneid", app_control_label("geneid", "Continuous feature(s)",
            "Typically genes or other continuous variables. Select one feature to show its values, or several to summarize using Combine features. The resulting values also determine which units pass the shared filters."), choices = features,
            selected = features[1], multiple = TRUE,
            options = list(plugins = list("remove_button"), maxOptions = 100, placeholder = "Search genes or metadata")),
        selectInput("multi_gene_method", app_control_label("multi_gene_method", "Combine features",
            tags$p("When several continuous features are selected, choose how to combine them within each sample:"),
            tags$ul(tags$li("Mean Z-score: standardize each feature and average its Z-scores."),
                tags$li("PC1: the first principal component of the standardized features."),
                tags$li("Proportion nonzero: the fraction of selected features with positive values.")),
            tags$p("With one feature, its original values are used. With Z-scores or PC1, a threshold of 0 hides nonpositive scores in all four panels.")),
            c("Mean Z-score" = "z_score", "PC1" = "pca", "Proportion nonzero" = "sparsity")),
        selectInput("assayname", app_control_label("assayname", "Assay",
            "Choose the gene values to visualize, such as counts or logcounts. This applies to selected genes; continuous metadata use their stored values."),
            choices = assayNames(spe), selected = tail(assayNames(spe), 1)),
        selectizeInput("reduced_name", app_control_label("reduced_name", "Embedding",
            "The first two dimensions of the selected embedding, such as UMAP or PCA, are shown in both embedding panels. Choose None to hide those panels. Their camera is linked separately from the spatial panels."), choices = c("None" = "", dimensions),
            selected = if (length(dimensions)) tail(dimensions, 1) else "",
            options = list(allowEmptyOption = TRUE)),
        tags$hr(),
        shinyWidgets::pickerInput("cluster_subset", app_control_label("cluster_subset", "Clusters to show",
            "Show only these categories of the selected discrete variable. This filter applies to all four interactive panels and the static grid. Clearing all categories hides all units. Scores are calculated before this filter, so changing the categories does not refit Z-scores or PCA."), choices = cluster_labels,
            selected = cluster_labels, multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
        numericInput("minCount", app_control_label("minCount", "Show values greater than",
            "Units with a continuous value or combined score less than or equal to this threshold are hidden from all four interactive panels and the static grid. You can enter any number and press Enter, including a negative threshold to retain negative Z-scores or PC1 scores."), 0, step = 1),
        numericInput("cap_percentile", app_control_label("cap_percentile", "Maximum percentile (0–1)",
            "When set below 1, values above the given percentile are capped, typically to control outliers. For example, 0.95 caps at the 95th percentile within each sample. Capping changes high values rather than removing those units; 1 leaves values uncapped."), 1, min = 0.001, max = 1, step = 0.05),
        actionButton("reset_filters", "Reset filters"),
        tags$details(class = "sl-appearance", tags$summary(tags$span("Appearance"),
            tags$span(class = "sl-appearance-show", "Show settings"),
            tags$span(class = "sl-appearance-hide", "Hide settings")),
            selectInput("boundary_style", app_control_label("boundary_style", "Cell/spot boundaries",
                "When boundaries are available, choose outlines or filled shapes. Boundaries appear as you zoom into spatial panels; outlines help compare them with histology. At wider views, units are represented by points."),
                c("Outlines" = "outline", "Filled" = "fill"), selected = "outline"),
            shinyWidgets::pickerInput("clustercolor", app_control_label("clustercolor", "Discrete palette",
                tags$p("Choose supplied colors from a column ending in _colors, or a palette from the paletteer R package. Only valid options are shown. Historical layer annotations retain their established layer colors."),
                tags$p("Visually explore palettes at ", tags$a(href = "https://emilhvitfeldt.github.io/r-color-palettes/discrete.html",
                    target = "_blank", rel = "noopener", "Palette Picker"), ".")), choices = palette$choices,
                selected = palette$selected, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
            checkboxInput("clustercolor_direction", "Reverse discrete palette", FALSE),
            selectInput("genecolor", app_control_label("genecolor", "Continuous palette",
                "Choose the color scale for continuous values. The viridis scale is color-blind friendly. Dark to light controls the palette direction."),
                c("viridis", "paper", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")),
            checkboxInput("genecolor_direction", "Dark to light", TRUE),
            sliderInput("alphalevel", app_control_label("alphalevel", "Unit opacity",
                "Adjust spatial units and embedding points from 0 (transparent) to 1 (full opacity). Units excluded by filters remain hidden."), 0, 1, 1, step = 0.05),
            sliderInput("image_opacity", app_control_label("image_opacity", "Image opacity",
                "Adjust the histology background in the interactive spatial panels, from 0 (hidden) to 1 (opaque). Unit opacity is controlled separately."), 0, 1, 0.8, step = 0.05),
            numericInput("pointsize_scale", app_control_label("pointsize_scale", "Point size adjustment",
                "Automatic sizing estimates local spacing separately for each sample's spatial and embedding panels at the full view. A value of 1 uses the suggested size; 2 doubles the radius and 0.5 halves it. Sizes stay stable while zooming or filtering and adapt when panels resize. Boundary shapes retain their true spatial size."),
                1, min = 0.1, max = 10, step = 0.1),
            numericInput("guidepointsize", app_control_label("guidepointsize", "Legend point size",
                "Controls the size of point symbols in the static grid's discrete legend. It does not change the spatial units themselves."), 2, min = 0.1, max = 10, step = 0.1)),
        tags$hr(),
        checkboxInput("dropNA", "Omit unlabeled units from CSV", TRUE),
        downloadButton("downloadData", "Download annotations"),
        fileInput("priorGuesses", "Import annotations", accept = ".csv", multiple = TRUE),
        helpText("CSV columns: sample_id, spot_name, ManualAnnotation. Later files overwrite earlier labels.")
    )
}

app_spot_controls_server <- function(input, session, spe, catalog) {
    initial <- isolate(spe())
    features <- c(golem::get_golem_options("spe_continuous_vars"), sort(rowData(initial)$gene_search))
    updateSelectizeInput(session, "geneid", choices = features, selected = features[1], server = TRUE)
    previous_variable <- reactiveVal(NULL)
    previous_choices <- character()
    observeEvent({req(input$cluster); list(input$cluster, spe()[[input$cluster]])}, {
        req(input$cluster)
        palette <- app_palette_choices(spe(), input$cluster)
        current <- isolate(input$clustercolor)
        changed <- !identical(input$cluster, previous_variable())
        selected <- if (length(current) == 1 && current %in% palette$choices) current else palette$selected
        if (changed && paste0(input$cluster, "_colors") %in% palette$choices) selected <- palette$selected
        updatePickerInput(session, "clustercolor", choices = palette$choices, selected = selected)
        labels <- as.character(spe()[[input$cluster]])
        labels[is.na(labels)] <- "(missing)"
        labels <- sort(unique(labels))
        old <- isolate(input$cluster_subset)
        choose <- if (changed || setequal(old, previous_choices)) labels else intersect(old, labels)
        previous_choices <<- labels
        previous_variable(input$cluster)
        updatePickerInput(session, "cluster_subset", choices = labels, selected = choose)
    }, ignoreNULL = FALSE)
    observeEvent(input$reset_filters, {
        updateNumericInput(session, "minCount", value = 0)
        updateNumericInput(session, "cap_percentile", value = 1)
        updatePickerInput(session, "cluster_subset", selected = previous_choices)
    })
    observeEvent(list(input$sample, catalog()), {
        entry <- catalog()[[input$sample]]
        ids <- names(entry$sources)
        current <- isolate(input$imageid)
        selected <- if (!is.null(current) && (identical(current, "") || current %in% ids)) current else entry$default
        updateSelectInput(session, "imageid", choices = c("None" = "", ids), selected = selected)
    })
    discrete <- reactive({
        req(input$cluster, input$clustercolor)
        app_discrete_colors(spe(), input$cluster, input$clustercolor, isTRUE(input$clustercolor_direction))
    })
    continuous <- reactive({
        req(input$genecolor)
        if (input$genecolor == "paper") {
            colors <- c("aquamarine4", "springgreen", "goldenrod", "red")
            if (isTRUE(input$genecolor_direction)) colors else rev(colors)
        } else viridisLite::viridis(21, option = input$genecolor,
            direction = if (isTRUE(input$genecolor_direction)) 1 else -1)
    })
    list(discrete = discrete, continuous = continuous)
}
