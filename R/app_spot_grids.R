# Static overviews share the viewer's measurements, filters and palettes.
# Build sample-grid layout controls and the explicit update/download actions.
app_spot_grid_ui <- function(spe) {
    tabPanel("Static grid", value = "grid",
        fluidRow(
            column(3, radioButtons("grid_kind", "Color by", c("Continuous" = "continuous", "Discrete" = "discrete"), inline = TRUE)),
            column(3, checkboxInput("grid_spatial", "Show histology", TRUE),
                checkboxInput("grid_side_by_side", "Image beside features", FALSE),
                checkboxInput("grid_fit_to_units", app_control_label("grid_fit_to_units", "Fit to cells/spots",
                    "With histology shown, fit each sample to all retained cells or spots with a small margin. Uncheck to show the full histology image. Without histology, the grid always fits the cells or spots. Expression and category filters do not change the framing. Click Update grid to apply."), TRUE)),
            column(3, numericInput("grid_nrow", "Rows", 1, min = 1, step = 1)),
            column(3, numericInput("grid_ncol", "Columns", 3, min = 1, step = 1))
        ),
        fluidRow(
            column(4, numericInput("grid_pointsize", app_control_label("grid_pointsize", "Point size (mm)",
                "Controls point size in the static grid and its downloaded plot. Smaller values help separate densely packed cells; larger values make sparse units easier to see. Use Interactive for detailed inspection of cell or spot boundaries."),
                if (app_spot_is_sfe(spe)) 0.35 else 2, min = 0.01, max = 10, step = 0.05))
        ),
        shiny::selectizeInput("grid_samples", app_control_label("grid_samples", "Samples",
            "Choose the samples and their display order. Remove and add sample IDs to change their order, then click Update grid. This selection is independent of the Sample selector in the sidebar."),
            choices = unique(spe$sample_id), selected = unique(spe$sample_id), multiple = TRUE,
            options = list(plugins = list("remove_button"))),
        actionButton("grid_update", "Update grid"), downloadButton("downloadPlotGrid", "Download plot"),
        helpText("Choose samples and click Update grid. Wide grids scroll horizontally. Points provide a quick overview; use Interactive for detailed inspection. Downloads match the displayed grid\u2019s dimensions and saved settings: PDF for smaller datasets, or a 300 dpi PNG for dense datasets."),
        uiOutput("grid_static"), textOutput("grid_warnings"), div(class = "app-plot-spacing")
    )
}

# Fit all retained units with padding in spatial coordinates, independent of platform or filters.
app_static_extent <- function(x, y, fallback = NULL, padding = 0.05) {
    extent <- c(min(x), min(y), max(x), max(y))
    span <- max(extent[3:4] - extent[1:2])
    if (span == 0) {
        if (!is.null(fallback)) return(fallback)
        span <- 1
    }
    extent + c(-1, -1, 1, 1) * span * padding
}

# ggplot2 4.x controls reversal explicitly; older versions reverse descending
# coordinate limits. Detect the API so development releases work as well.
app_static_coord <- function(extent, y_down) {
    args <- list(xlim = extent[c(1, 3)], ylim = extent[c(2, 4)], expand = FALSE)
    if ("reverse" %in% names(formals(ggplot2::coord_cartesian))) {
        args$reverse <- if (y_down) "y" else "none"
    } else if (y_down) {
        args$ylim <- rev(args$ylim)
    }
    do.call(ggplot2::coord_fixed, args)
}

# Draw one sample using the shared scores and filters, with optional registered histology.
app_static_spatial <- function(spe, sample, cluster, gene, clusters, kind, colors,
    catalog, image_id, spatial, point_size, guide_size, opacity, side_by_side, fit_to_units = TRUE) {
    d <- app_spot_frame(spe, sample, cluster, gene, clusters)
    source <- app_image_source(catalog, sample, image_id)
    # Hidden histology should neither load a raster nor determine the plot's framing.
    image <- if (spatial) app_image_raster(spe, sample,
        app_image_source(catalog, sample, image_id, thumbnail = TRUE)) else NULL
    # Native SFE geometries use Cartesian Y-up unless an image registration says otherwise.
    y_down <- app_spot_y_down(spe, if (is.null(source)) app_image_source(catalog, sample) else source)
    # Change only the viewport; the raster stays anchored in its original coordinate frame.
    image_extent <- if (is.null(image)) NULL else image$extent
    extent <- if (fit_to_units || is.null(image)) {
        app_static_extent(d$x, d$y, fallback = image_extent)
    } else image_extent
    background <- ggplot() + app_static_coord(extent, y_down)
    if (!is.null(image)) {
        # Keep raster rows upright; the coordinate system places image-space
        # points with increasing Y toward the bottom of the same extent.
        background <- background + annotation_raster(image$image,
            xmin = image_extent[1], xmax = image_extent[3], ymin = image_extent[2], ymax = image_extent[4])
    }
    plot <- if (spatial && !side_by_side) background else ggplot() + app_static_coord(extent, y_down)
    keep <- d$eligible
    values <- if (kind == "continuous") d$value else d$labels
    alpha <- if (is.finite(opacity)) opacity else 1
    df <- data.frame(x = d$x[keep], y = d$y[keep], value = values[keep])
    # A zero-width stroke can become a black hairline in PDF viewers. Remove
    # the outline explicitly so dense overview points retain their fill colors.
    x <- y <- value <- NULL # ggplot2 data-mask bindings
    plot <- plot + geom_point(data = df, aes(x = x, y = y, fill = value),
        shape = 21, colour = "transparent", stroke = 0, size = point_size, alpha = alpha)
    if (kind == "continuous") {
        plot <- plot + scale_fill_gradientn(colours = colors$continuous,
            limits = app_spot_domain(d$value), name = paste(gene$geneid, collapse = ", "))
    } else {
        plot <- plot + scale_fill_manual(values = get_colors(colors$discrete, d$labels),
            name = cluster, drop = FALSE) + guides(fill = guide_legend(override.aes = list(size = guide_size)))
    }
    theme <- theme_void(base_size = 12) + theme(plot.title = element_text(hjust = 0.5))
    plot <- plot + theme + ggtitle(sample)
    if (spatial && side_by_side && !is.null(image)) plot <- cowplot::plot_grid(background + theme, plot, nrow = 1)
    list(p = plot, warnings = d$warnings, points = sum(keep))
}

# Snapshot grid settings on Update so the displayed plot and its download stay consistent.
app_spot_grids_server <- function(input, output, spe, colors, catalog, session) {
    grid <- eventReactive(input$grid_update, {
        req(length(input$grid_samples) > 0, input$cluster, length(input$geneid) > 0)
        # Validate the requested layout and expand its rows to fit every selected sample.
        dimensions <- c(input$grid_nrow, input$grid_ncol)
        validate(need(length(dimensions) == 2L && all(is.finite(dimensions)) &&
            all(dimensions >= 1 & dimensions <= .Machine$integer.max & dimensions == floor(dimensions)),
            "Rows and columns must be positive whole numbers."))
        rows <- as.integer(input$grid_nrow)
        cols <- as.integer(input$grid_ncol)
        validate(need(length(input$grid_pointsize) == 1L && is.finite(input$grid_pointsize) && input$grid_pointsize > 0,
            "Enter a positive point size."))
        rows <- max(rows, ceiling(length(input$grid_samples) / cols))
        # Build all sample panels from one saved set of feature and appearance settings.
        gene <- app_gene_settings(input, colors$continuous)
        plots <- lapply(input$grid_samples, function(sample) app_static_spatial(
            spe(), sample, input$cluster, gene, as.character(input$cluster_subset), input$grid_kind,
            list(discrete = colors$discrete(), continuous = colors$continuous()), catalog(),
            input$imageid, isTRUE(input$grid_spatial), input$grid_pointsize, input$guidepointsize,
            input$alphalevel, isTRUE(input$grid_side_by_side), fit_to_units = isTRUE(input$grid_fit_to_units)))
        list(p = cowplot::plot_grid(plotlist = lapply(plots, `[[`, "p"), nrow = rows, ncol = cols),
            warnings = unique(unlist(lapply(plots, `[[`, "warnings"))), rows = rows, cols = cols,
            # Choose from the points actually drawn in this saved grid. The
            # threshold approximates the PDF / 300 dpi PNG file-size crossover.
            format = if (mean(vapply(plots, `[[`, numeric(1), "points")) < 50000) "pdf" else "png",
            side_by_side = isTRUE(input$grid_spatial) && isTRUE(input$grid_side_by_side),
            fit_to_units = isTRUE(input$grid_fit_to_units),
            stem = paste("static_grid", input$grid_kind,
                if (input$grid_kind == "continuous") paste(input$geneid, collapse = "_") else input$cluster,
                paste(input$grid_samples, collapse = "_"), sep = "_"))
    }, ignoreNULL = FALSE)
    output$grid_static <- renderUI({
        settings <- grid()
        min_width <- 360 * settings$cols * if (settings$side_by_side) 2 else 1
        div(style = "overflow-x: auto; max-width: 100%;",
            div(style = paste0("min-width: ", min_width, "px;"),
                plotOutput("spot_grid", width = "100%", height = 500 * settings$rows)))
    })
    output$spot_grid <- renderPlot(print(grid()$p), res = 96, execOnResize = TRUE)
    output$grid_warnings <- renderText(app_warning_text(grid))
    output$downloadPlotGrid <- downloadHandler(
        filename = function() app_filename(grid()$stem, grid()$format),
        content = function(file) {
            settings <- grid()
            # renderPlot uses 96 CSS pixels per inch. Match the full displayed
            # canvas (including horizontally scrolled columns), then increase
            # only raster resolution so point sizes and layout stay in sync.
            width <- session$clientData$output_spot_grid_width
            height <- session$clientData$output_spot_grid_height
            req(is.finite(width), width > 0, is.finite(height), height > 0)
            width <- width / 96
            height <- height / 96
            if (settings$format == "pdf") {
                grDevices::pdf(file, width = width, height = height, useDingbats = FALSE)
            } else {
                grDevices::png(file, width = width, height = height,
                    units = "in", res = 300, bg = "white")
            }
            on.exit(grDevices::dev.off(), add = TRUE)
            print(settings$p)
        }
    )
    list(grid = grid)
}
