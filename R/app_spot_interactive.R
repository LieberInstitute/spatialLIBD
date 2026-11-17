# One linked viewer: two spatial colorings and, when available, two embeddings.
app_spot_interactive_ui <- function() {
    tabPanel("Interactive", value = "interactive",
        div(class = "sl-viewer-toolbar", role = "toolbar", `aria-label` = "Spatial viewer tools",
            tags$button(type = "button", class = "btn btn-default active", `data-viewer-tool` = "pan", "Pan"),
            tags$button(type = "button", class = "btn btn-default", `data-viewer-tool` = "select", "Select"),
            tags$button(type = "button", class = "btn btn-default", `data-viewer-tool` = "lasso", "Lasso"),
            tags$button(type = "button", class = "btn btn-default", `data-viewer-action` = "reset", "Reset view"),
            tags$button(type = "button", class = "btn btn-default", `data-viewer-action` = "clear", "Clear selection"),
            tags$span(class = "sl-viewer-status", role = "status", "Loading spatial data…")
        ),
        div(id = "spot_viewer", class = "sl-viewer", tabindex = "0", `aria-label` = "Linked spatial and embedding plots",
            div(class = "sl-viewer-canvas"), tags$svg(class = "sl-viewer-lasso", `aria-hidden` = "true"),
            div(class = "sl-viewer-error", role = "alert")
        ),
        div(class = "sl-viewer-legends"),
        textOutput("viewer_filter_summary"), textOutput("viewer_warnings"),
        helpText("Spatial panels share a view; embedding panels share a separate view. Selections link all panels. Shift adds to a selection; Alt removes. Boundaries appear as you zoom in."),
        fluidRow(
            column(4, textInput("label_manual_ann", "Manual annotation label", "Your Guess")),
            column(4, checkboxInput("label_click", "Annotate individual units on click", FALSE)),
            column(4, actionButton("update_manual_ann", "Label selected units"),
                actionButton("undo_manual_ann", "Undo last annotation"))
        ), verbatimTextOutput("click"), div(class = "app-plot-spacing")
    )
}

app_spot_interactive_server <- function(input, output, session, spe, colors, resources, catalog) {
    initial <- isolate(spe())
    datasets <- new.env(parent = emptyenv())
    revision <- 0L
    geometry_files <- new.env(parent = emptyenv())
    frame <- reactive({
        req(input$sample, input$cluster, length(input$geneid) > 0,
            input$assayname, input$multi_gene_method, input$cap_percentile, !is.null(input$minCount))
        app_spot_frame(spe(), input$sample, input$cluster,
            app_gene_settings(input, function() NULL), as.character(input$cluster_subset))
    })
    gene_data <- reactive({
        d <- frame()
        data.frame(key = d$key[d$eligible], COUNT = d$value[d$eligible])
    })
    output$viewer_filter_summary <- renderText({
        d <- frame()
        paste0(format(sum(d$eligible), big.mark = ","), " of ", format(length(d$key), big.mark = ","),
            " units shown · value > ", input$minCount,
            if (input$cap_percentile < 1) paste0(" · capped at ", 100 * input$cap_percentile, "th percentile"))
    })
    output$viewer_warnings <- renderText(paste(frame()$warnings, collapse = "; "))
    dataset <- reactive({
        req(input$sample)
        reduced <- input$reduced_name
        if (is.null(reduced)) reduced <- ""
        key <- paste(input$sample, reduced, sep = ":")
        if (exists(key, datasets, inherits = FALSE)) return(datasets[[key]])
        i <- app_spot_indices(initial, input$sample)
        xy <- app_spot_coordinates(initial, i)
        embedding <- NULL
        if (reduced %in% reducedDimNames(initial)) {
            dims <- reducedDim(initial, reduced)
            if (ncol(dims) >= 2L) embedding <- unname(dims[i, 1:2, drop = FALSE])
        }
        source <- app_image_source(catalog(), input$sample)
        y_down <- app_spot_y_down(initial, source)
        path <- tempfile("units-", resources$root, ".json")
        jsonlite::write_json(list(keys = I(initial$key[i]), x = I(xy[, 1]), y = I(xy[, 2]),
            embedding = embedding, embeddingName = reduced), path, auto_unbox = TRUE, digits = 12, na = "null")
        if (!exists(input$sample, geometry_files, inherits = FALSE)) {
            prepared <- S4Vectors::metadata(initial)$app_boundaries[[input$sample]]
            geometry_files[[input$sample]] <- if (is.null(prepared)) "" else resources$asset(prepared)
        }
        answer <- list(id = key, sample = input$sample, url = resources$url(path),
            geometry = geometry_files[[input$sample]], yDown = y_down,
            hasEmbedding = !is.null(embedding), embeddingName = reduced)
        datasets[[key]] <- answer
        answer
    })
    feature_files <- character()
    features <- reactive({
        d <- frame()
        meta <- dataset()
        domain <- app_spot_domain(d$value)
        continuous <- scales::col_numeric(colors$continuous(), domain = domain, na.color = "#CCCCCC40")(d$value)
        palette <- colors$discrete()
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
        if (length(feature_files) > 6L) {
            unlink(feature_files[seq_len(length(feature_files) - 6L)])
            feature_files <<- tail(feature_files, 6L)
        }
        revision <<- revision + 1L
        list(
            dataset = meta, features = resources$url(path), revision = revision,
            domain = domain, palette = I(colors$continuous()),
            continuousTitle = paste(input$geneid, collapse = ", "), discreteTitle = input$cluster,
            discreteLegend = lapply(legend_labels, function(l) list(label = l,
                color = unname(color_map[l]))),
            shown = sum(d$eligible), total = length(d$key))
    })
    appearance <- reactive({
        req(input$sample)
        source <- app_image_source(catalog(), input$sample, input$imageid)
        list(sample = input$sample, pointSizeScale = input$pointsize_scale,
            opacity = if (is.finite(input$alphalevel)) input$alphalevel else 1,
            imageOpacity = input$image_opacity, boundaryStyle = input$boundary_style,
            image = app_image_browser(spe(), input$sample, source, resources))
    })
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
