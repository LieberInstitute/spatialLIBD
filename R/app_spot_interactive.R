# One linked viewer: two spatial colorings and, when available, two embeddings.
# Build linked viewer panels, navigation tools, and the manual annotation actions.
app_spot_interactive_ui <- function(controls) {
    tabPanel("Interactive", value = "interactive", sidebarLayout(
        sidebarPanel(width = 3, controls),
        mainPanel(width = 9,
            div(class = "sl-viewer-toolbar", role = "toolbar", `aria-label` = "Spatial viewer tools",
                tags$button(type = "button", class = "btn btn-default active", `data-viewer-tool` = "pan", "Pan"),
                tags$button(type = "button", class = "btn btn-default", `data-viewer-tool` = "select", "Select"),
                tags$button(type = "button", class = "btn btn-default", `data-viewer-tool` = "lasso", "Lasso"),
                tags$button(type = "button", class = "btn btn-default", `data-viewer-action` = "reset", "Reset view"),
                tags$button(type = "button", class = "btn btn-default", `data-viewer-action` = "clear", "Clear selection")),
            div(class = "sl-viewer-status", role = "status", "Loading spatial data\u2026"),
            div(id = "spot_viewer", class = "sl-viewer", tabindex = "0", `aria-label` = "Linked spatial and embedding plots",
                div(class = "sl-viewer-canvas"), tags$svg(class = "sl-viewer-lasso", `aria-hidden` = "true"),
                div(class = "sl-viewer-error", role = "alert")),
            div(class = "sl-viewer-legends"),
            textOutput("viewer_filter_summary"), textOutput("viewer_warnings"),
            helpText("Spatial panels share a view; embedding panels share a separate view. Selections link all panels. Shift adds to a selection; Alt removes. Boundaries appear as you zoom in."),
            app_annotations_ui(), verbatimTextOutput("click"), div(class = "app-plot-spacing"))))
}

# Cache sample geometry and send reactive feature and appearance updates to the viewer.
app_spot_interactive_server <- function(input, output, session, spe, controls, resources, catalog) {
    view <- controls$input
    initial <- isolate(spe())
    datasets <- new.env(parent = emptyenv())
    revision <- 0L
    geometry_files <- new.env(parent = emptyenv())
    # Share one eligible set between browser rendering, summaries, and annotation actions.
    frame <- reactive({
        req(view$sample, view$cluster, length(view$geneid) > 0,
            view$assayname, view$multi_gene_method, view$cap_percentile, !is.null(view$minCount))
        app_spot_frame(spe(), view$sample, view$cluster,
            controls$gene(), as.character(view$cluster_subset))
    })
    gene_data <- reactive({
        d <- frame()
        data.frame(key = d$key[d$eligible], COUNT = d$value[d$eligible])
    })
    output$viewer_filter_summary <- renderText({
        d <- frame()
        paste0(format(sum(d$eligible), big.mark = ","), " of ", format(length(d$key), big.mark = ","),
            " units shown \u00b7 value > ", view$minCount,
            if (view$cap_percentile < 1) paste0(" \u00b7 capped at ", 100 * view$cap_percentile, "th percentile"))
    })
    output$viewer_warnings <- renderText(paste(frame()$warnings, collapse = "; "))
    # Cache positions and embeddings by sample; gene or palette changes reuse these files.
    dataset <- reactive({
        req(view$sample)
        reduced <- view$reduced_name
        if (is.null(reduced)) reduced <- ""
        key <- paste(view$sample, reduced, sep = ":")
        if (exists(key, datasets, inherits = FALSE)) return(datasets[[key]])
        i <- app_spot_indices(initial, view$sample)
        xy <- app_spot_coordinates(initial, i)
        embedding <- NULL
        if (reduced %in% reducedDimNames(initial)) {
            dims <- reducedDim(initial, reduced)
            if (ncol(dims) >= 2L) embedding <- unname(dims[i, 1:2, drop = FALSE])
        }
        source <- app_image_source(catalog(), view$sample)
        y_down <- app_spot_y_down(initial, source)
        path <- tempfile("units-", resources$root, ".json")
        jsonlite::write_json(list(keys = I(initial$key[i]), x = I(xy[, 1]), y = I(xy[, 2]),
            embedding = embedding, embeddingName = reduced), path, auto_unbox = TRUE, digits = 12, na = "null")
        if (!exists(view$sample, geometry_files, inherits = FALSE)) {
            prepared <- S4Vectors::metadata(initial)$app_boundaries[[view$sample]]
            geometry_files[[view$sample]] <- if (is.null(prepared)) "" else resources$asset(prepared)
        }
        answer <- list(id = key, sample = view$sample, url = resources$url(path),
            geometry = geometry_files[[view$sample]], yDown = y_down,
            hasEmbedding = !is.null(embedding), embeddingName = reduced)
        datasets[[key]] <- answer
        answer
    })
    # Write only changing scores, category IDs, colors, and visibility to the binary payload.
    feature_files <- character()
    features <- reactive({
        d <- frame()
        meta <- dataset()
        domain <- app_spot_domain(d$value)
        continuous <- scales::col_numeric(controls$continuous(), domain = domain, na.color = "#CCCCCC40")(d$value)
        palette <- controls$discrete()
        legend_labels <- unique(d$labels)
        color_map <- get_colors(palette, d$labels)
        discrete <- color_map[d$labels]
        discrete[is.na(discrete)] <- "#CCCCCC40"
        path <- tempfile("features-", resources$root, ".bin")
        con <- file(path, "wb")
        tryCatch({
            writeBin(as.numeric(d$value), con, size = 4L, endian = "little")
            writeBin(as.integer(match(d$labels, legend_labels) - 1L), con, size = 4L, endian = "little")
            writeBin(as.raw(app_spot_rgba(continuous)), con)
            writeBin(as.raw(app_spot_rgba(discrete)), con)
            writeBin(as.raw(d$eligible), con)
        }, finally = close(con))
        feature_files <<- c(feature_files, path)
        # Keep a small recent history for in-flight browser requests without growing indefinitely.
        if (length(feature_files) > 6L) {
            unlink(feature_files[seq_len(length(feature_files) - 6L)])
            feature_files <<- utils::tail(feature_files, 6L)
        }
        # Tag each payload so annotation clicks can be checked against the current view.
        revision <<- revision + 1L
        list(
            dataset = meta, features = resources$url(path), revision = revision,
            domain = domain, palette = I(controls$continuous()),
            continuousTitle = paste(view$geneid, collapse = ", "), discreteTitle = view$cluster,
            discreteLegend = lapply(legend_labels, function(l) list(label = l,
                color = unname(color_map[l]))),
            shown = sum(d$eligible), total = length(d$key))
    })
    # Keep image and styling updates separate so they do not rewrite feature data.
    appearance <- reactive({
        req(view$sample)
        source <- app_image_source(catalog(), view$sample, view$imageid)
        list(sample = view$sample, pointSizeScale = view$pointsize_scale,
            opacity = if (is.finite(view$alphalevel)) view$alphalevel else 1,
            imageOpacity = view$image_opacity, boundaryStyle = view$boundary_style,
            image = app_image_browser(spe(), view$sample, source, resources))
    })
    # Wait for browser initialization before sending data or exposing rendering errors.
    observe({
        req(isTRUE(input$viewer_ready))
        tryCatch({
            payload <- features()
            session$sendCustomMessage("spatiallibd:viewer", c(payload, isolate(appearance())))
        }, error = function(e) {
            session$sendCustomMessage("spatiallibd:error", list(message = conditionMessage(e)))
        })
    }, priority = 10)
    observe({
        req(isTRUE(input$viewer_ready))
        tryCatch(session$sendCustomMessage("spatiallibd:appearance", appearance()),
            error = function(e) session$sendCustomMessage("spatiallibd:error", list(message = conditionMessage(e))))
    })
    list(gene_data = gene_data, frame = frame, revision = function() revision)
}
