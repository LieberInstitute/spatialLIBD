# Shared selections; filters operate on one eligible set for all four panels.
app_spot_controls_ui <- function(spe, spe_discrete_vars, spe_continuous_vars,
    default_cluster, auto_crop_default, datatype) {
    catalog <- app_image_catalog(spe)
    samples <- unique(spe$sample_id)
    images <- app_image_choices(catalog)
    dimensions <- sort(reducedDimNames(spe))
    features <- c(spe_continuous_vars, sort(rowData(spe)$gene_search))
    palette <- app_palette_choices(spe, default_cluster)
    cluster_labels <- as.character(spe[[default_cluster]])
    cluster_labels[is.na(cluster_labels)] <- "(missing)"
    cluster_labels <- sort(unique(cluster_labels))
    sidebarPanel(width = 3,
        shinyWidgets::pickerInput("sample", "Sample", choices = samples,
            options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        selectizeInput("imageid", "Histology image", choices = c("None" = "", images),
            selected = if (length(catalog[[samples[1]]]$default)) catalog[[samples[1]]]$default else "",
            options = list(allowEmptyOption = TRUE)),
        shinyWidgets::pickerInput("cluster", "Discrete variable", choices = spe_discrete_vars,
            selected = default_cluster, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        shinyWidgets::pickerInput("geneid", "Continuous feature(s)", choices = features,
            selected = features[1], multiple = TRUE, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        selectInput("multi_gene_method", "Combine features", c("Mean Z-score" = "z_score", "PC1" = "pca", "Proportion nonzero" = "sparsity")),
        selectInput("assayname", "Assay", choices = assayNames(spe), selected = tail(assayNames(spe), 1)),
        selectizeInput("reduced_name", "Embedding", choices = c("None" = "", dimensions),
            selected = if (length(dimensions)) tail(dimensions, 1) else "",
            options = list(allowEmptyOption = TRUE)),
        tags$hr(),
        shinyWidgets::pickerInput("cluster_subset", "Clusters to show", choices = cluster_labels,
            selected = cluster_labels, multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
        numericInput("minCount", "Show values greater than", 0, step = 1),
        numericInput("cap_percentile", "Maximum percentile (0–1)", 1, min = 0.001, max = 1, step = 0.05),
        actionButton("reset_filters", "Reset filters"),
        tags$details(tags$summary("Appearance"),
            selectInput("boundary_style", "Cell/spot boundaries",
                c("Filled" = "fill", "Outlines" = "outline", "Filled with outlines" = "both")),
            shinyWidgets::pickerInput("clustercolor", "Discrete palette", choices = palette$choices,
                selected = palette$selected, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
            checkboxInput("clustercolor_direction", "Reverse discrete palette", FALSE),
            selectInput("genecolor", "Continuous palette", c("viridis", "paper", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")),
            checkboxInput("genecolor_direction", "Dark to light", TRUE),
            numericInput("alphalevel", "Unit opacity (blank = automatic)", NA, min = 0, max = 1, step = 0.1),
            sliderInput("image_opacity", "Image opacity", 0, 1, 0.8, step = 0.05),
            numericInput("pointsize", "Interactive point size", 2, min = 0.1, max = 10, step = 0.1),
            numericInput("guidepointsize", "Legend point size", 2, min = 0.1, max = 10, step = 0.1)),
        tags$hr(),
        checkboxInput("dropNA", "Omit unlabeled units from CSV", TRUE),
        downloadButton("downloadData", "Download annotations"),
        fileInput("priorGuesses", "Import annotations", accept = ".csv", multiple = TRUE),
        helpText("CSV columns: sample_id, spot_name, ManualAnnotation. Later files overwrite earlier labels.")
    )
}

app_spot_controls_server <- function(input, session, spe, catalog) {
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
