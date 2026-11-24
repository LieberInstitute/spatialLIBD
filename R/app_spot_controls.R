# Reuse control definitions and defaults while keeping each view's state independent.
app_spot_controls_ui <- function(id, spe, spe_discrete_vars, spe_continuous_vars,
    default_cluster, mode = c("interactive", "grid")) {
    mode <- match.arg(mode)
    ns <- NS(id)
    catalog <- app_image_catalog(spe)
    samples <- unique(spe$sample_id)
    dimensions <- sort(reducedDimNames(spe))
    labels <- as.character(spe[[default_cluster]])
    labels[is.na(labels)] <- "(missing)"
    tagList(
        if (mode == "interactive") tagList(
            shinyWidgets::pickerInput(ns("sample"), app_control_label(ns("sample"), "Sample",
                "Choose the sample shown in the linked interactive panels."), choices = samples,
                options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
            selectizeInput(ns("imageid"), app_control_label(ns("imageid"), "Histology image",
                "Choose the background image, or None to hide it. Full-resolution pyramids reveal more detail as you zoom. Select edited_imaged to use the low-resolution edits at every zoom level."),
                choices = c("None" = "", app_image_choices(catalog)),
                selected = if (length(catalog[[samples[1]]]$default)) catalog[[samples[1]]]$default else "",
                options = list(allowEmptyOption = TRUE))),
        app_spot_features_ui(ns, spe, spe_discrete_vars, spe_continuous_vars, default_cluster),
        if (mode == "interactive") selectizeInput(ns("reduced_name"), app_control_label(ns("reduced_name"), "Embedding",
            "Show the first two dimensions of this embedding in the lower panels. None hides those panels. Their camera is linked separately from the spatial panels."),
            choices = c("None" = "", dimensions), selected = if (length(dimensions)) utils::tail(dimensions, 1) else "",
            options = list(allowEmptyOption = TRUE)),
        tags$hr(), app_spot_filters_ui(ns, sort(unique(labels))),
        app_control_section("Appearance",
            app_spot_appearance_ui(ns, spe, default_cluster, mode)))
}

# Feature selection and scoring work the same way in each plotting tab.
app_spot_features_ui <- function(ns, spe, discrete_vars, continuous_vars, default_cluster) {
    feature <- if (length(continuous_vars)) continuous_vars[1] else sort(rowData(spe)$gene_search)[1]
    tagList(
        shinyWidgets::pickerInput(ns("cluster"), app_control_label(ns("cluster"), "Discrete variable",
            "Choose the categories used for discrete coloring and category filters in this tab."),
            choices = discrete_vars, selected = default_cluster, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        selectizeInput(ns("geneid"), app_control_label(ns("geneid"), "Continuous feature(s)",
            "Choose genes or continuous metadata. Multiple features are combined using the method below. These values also determine which cells or spots pass the threshold, including when coloring by discrete categories."),
            choices = feature, selected = feature, multiple = TRUE,
            options = list(plugins = list("remove_button"), maxOptions = 100, placeholder = "Search genes or metadata")),
        selectInput(ns("multi_gene_method"), app_control_label(ns("multi_gene_method"), "Combine features",
            tags$p("When several continuous features are selected, combine them within each sample:"),
            tags$ul(tags$li("Mean Z-score: standardize each feature and average its Z-scores."),
                tags$li("PC1: the first principal component of the standardized features."),
                tags$li("Proportion nonzero: the fraction of selected features with positive values.")),
            tags$p("One feature uses its original values. With Z-scores or PC1, a threshold of 0 hides nonpositive scores.")),
            c("Mean Z-score" = "z_score", "PC1" = "pca", "Proportion nonzero" = "sparsity")),
        selectInput(ns("assayname"), app_control_label(ns("assayname"), "Assay",
            "Choose the gene values, such as counts or logcounts. Continuous metadata use their stored values."),
            choices = assayNames(spe), selected = utils::tail(assayNames(spe), 1)))
}

# Filters belong to their view; scoring precedes category masking.
app_spot_filters_ui <- function(ns, labels) {
    tagList(
        shinyWidgets::pickerInput(ns("cluster_subset"), app_control_label(ns("cluster_subset"), "Clusters to show",
            "Show only these categories in this tab. Clearing all categories hides all units. Scores are calculated before this filter, so category changes do not refit Z-scores or PCA."),
            choices = labels, selected = labels, multiple = TRUE,
            options = shinyWidgets::pickerOptions(actionsBox = TRUE, liveSearch = TRUE)),
        numericInput(ns("minCount"), app_control_label(ns("minCount"), "Show values greater than",
            "Hide units with a continuous value or combined score at or below this threshold, including in discrete plots. Negative thresholds can retain negative Z-scores or PC1 scores."), 0, step = 1),
        numericInput(ns("cap_percentile"), app_control_label(ns("cap_percentile"), "Maximum percentile (0\u20131)",
            "Cap values above this percentile within each sample. For example, 0.95 caps the top 5%. This changes high values rather than removing those units; 1 leaves values uncapped."),
            1, min = 0.001, max = 1, step = 0.05),
        actionButton(ns("reset_filters"), "Reset filters"))
}

# Show only the appearance settings that the view actually uses.
app_spot_appearance_ui <- function(ns, spe, default_cluster, mode) {
    palette <- app_palette_choices(spe, default_cluster)
    discrete <- tagList(
        shinyWidgets::pickerInput(ns("clustercolor"), app_control_label(ns("clustercolor"), "Discrete palette",
            tags$p("Choose supplied colors from a matching _colors column or a paletteer palette. Historical layer annotations retain their established colors."),
            tags$p("Explore palettes at ", tags$a(href = "https://emilhvitfeldt.github.io/r-color-palettes/discrete.html",
                target = "_blank", rel = "noopener", "Palette Picker"), ".")),
            choices = palette$choices, selected = palette$selected, options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
        checkboxInput(ns("clustercolor_direction"), "Reverse discrete palette", FALSE),
        if (mode == "grid") numericInput(ns("guidepointsize"), app_control_label(ns("guidepointsize"), "Legend symbol size (mm)",
            "Set the size of discrete legend symbols. This does not change the plotted cells or spots."), 2, min = 0.1, max = 10, step = 0.1))
    continuous <- tagList(
        selectInput(ns("genecolor"), app_control_label(ns("genecolor"), "Continuous palette",
            "Choose the color scale for continuous values. Dark to light controls its direction."),
            c("viridis", "paper", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo")),
        checkboxInput(ns("genecolor_direction"), "Dark to light", TRUE))
    tagList(
        if (mode == "grid") conditionalPanel("input.grid_kind === 'discrete'", discrete) else discrete,
        if (mode == "grid") conditionalPanel("input.grid_kind === 'continuous'", continuous) else continuous,
        sliderInput(ns("alphalevel"), app_control_label(ns("alphalevel"), "Unit opacity",
            "Adjust plotted cells or spots from 0 (transparent) to 1 (opaque). Filtered units remain hidden."), 0, 1, 1, step = 0.05),
        if (mode == "interactive") tagList(
            selectInput(ns("boundary_style"), app_control_label(ns("boundary_style"), "Cell/spot boundaries",
                "When boundaries are available, show outlines or filled shapes as you zoom into spatial panels. Wider views use points."),
                c("Outlines" = "outline", "Filled" = "fill"), selected = "outline"),
            sliderInput(ns("image_opacity"), app_control_label(ns("image_opacity"), "Image opacity",
                "Adjust the histology background from 0 (hidden) to 1 (opaque), independently of unit opacity."), 0, 1, 0.8, step = 0.05),
            numericInput(ns("pointsize_scale"), app_control_label(ns("pointsize_scale"), "Point size",
                "Multiply the automatically estimated point radius: 1 uses the suggested size, 2 doubles it, and 0.5 halves it. Sizes stay stable while zooming or filtering. Boundary shapes retain their true spatial size."),
                1, min = 0.1, max = 10, step = 0.1)))
}

# Maintain valid feature, palette, category, and image selections as the data change.
app_spot_controls_server <- function(id, spe, catalog, mode = c("interactive", "grid")) {
    mode <- match.arg(mode)
    moduleServer(id, function(input, output, session) {
        initial <- isolate(spe())
        features <- c(golem::get_golem_options("spe_continuous_vars"), sort(rowData(initial)$gene_search))
        updateSelectizeInput(session, "geneid", choices = features, selected = features[1], server = TRUE)
        # Keep deliberate category subsets across updates, but reset them for a new variable.
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
        # Retain a valid image selection, including an explicit request to hide histology.
        if (mode == "interactive") observeEvent(list(input$sample, catalog()), {
            entry <- catalog()[[input$sample]]
            ids <- names(entry$sources)
            current <- isolate(input$imageid)
            selected <- if (!is.null(current) && (identical(current, "") || current %in% ids)) current else entry$default
            updateSelectInput(session, "imageid", choices = c("None" = "", ids), selected = selected)
        })
        # Return this view's settings and palettes without coupling it to another tab.
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
        list(input = input, discrete = discrete, continuous = continuous,
            gene = reactive(app_gene_settings(input, function() NULL)))
    })
}
