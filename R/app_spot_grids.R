# Static overviews share the viewer's measurements, filters and palettes.
# Build sample-grid layout controls and the explicit update/download actions.
app_spot_grid_ui <- function(spe, controls) {
    tabPanel("Static grid", value = "grid", sidebarLayout(
        sidebarPanel(width = 3,
            shiny::selectizeInput("grid_samples", app_control_label("grid_samples", "Samples",
                "Choose the samples and their display order. Remove and add IDs to reorder them, then click Update grid."),
                choices = unique(spe$sample_id), selected = unique(spe$sample_id), multiple = TRUE,
                options = list(plugins = list("remove_button"))),
            numericInput("grid_ncol", app_control_label("grid_ncol", "Columns",
                "Rows are calculated automatically. Each spatial panel keeps a fixed 300 by 300 pixel area; wide grids scroll horizontally."),
                min(3, length(unique(spe$sample_id))), min = 1, max = length(unique(spe$sample_id)), step = 1),
            radioButtons("grid_kind", "Color by", c("Continuous" = "continuous", "Discrete" = "discrete"), inline = TRUE),
            controls,
            tags$hr(), checkboxInput("grid_spatial", "Show histology", TRUE),
            conditionalPanel("input.grid_spatial", selectInput("grid_imageid", app_control_label("grid_imageid", "Histology image",
                "Choose a low-resolution image for the selected samples. If that image is unavailable in a sample, its default low-resolution image is used."), choices = character()),
                checkboxInput("grid_side_by_side", "Image beside features", FALSE),
                checkboxInput("grid_fit_to_units", app_control_label("grid_fit_to_units", "Fit to cells/spots",
                    "Fit each sample to retained cells or spots with a small margin. Uncheck to show the full histology image. Without histology, the grid always fits cells or spots. Expression and category filters do not change framing."), TRUE)),
            numericInput("grid_pointsize", app_control_label("grid_pointsize", "Point size (mm)",
                "Set the size of plotted cells or spots in the grid and its download."),
                if (app_spot_is_sfe(spe)) 0.35 else 2, min = 0.01, max = 10, step = 0.05),
            checkboxInput("grid_shared_legend", app_control_label("grid_shared_legend", "One shared legend",
                "Use one legend covering all selected samples, or uncheck for sample-specific legends. Shared legends include all displayed categories or one continuous color range."), TRUE),
            actionButton("grid_update", "Update grid"), downloadButton("downloadPlotGrid", "Download plot"),
            helpText("Click Update grid to apply settings. Downloads use the displayed grid's saved settings: PDF for smaller datasets, or a 300 dpi PNG for dense datasets.")),
        mainPanel(width = 9, uiOutput("grid_static"), textOutput("grid_warnings"), div(class = "app-plot-spacing"))))
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
    # Solid circles avoid a separate outline: alpha can turn a "transparent"
    # border on fillable circles white, producing hairlines in PDF exports.
    x <- y <- value <- NULL # ggplot2 data-mask bindings
    plot <- plot + geom_point(data = df, aes(x = x, y = y, colour = value),
        shape = 16, stroke = 0, size = point_size, alpha = alpha, show.legend = TRUE)
    if (kind == "continuous") {
        plot <- plot + scale_colour_gradientn(colours = colors$continuous,
            limits = app_spot_domain(d$value), name = paste(gene$geneid, collapse = ", "))
    } else {
        plot <- plot + scale_colour_manual(values = get_colors(colors$discrete, d$labels),
            name = cluster, drop = FALSE) + guides(colour = guide_legend(override.aes = list(size = guide_size)))
    }
    theme <- theme_void(base_size = 12) + theme(plot.title = element_text(hjust = 0.5))
    plot <- plot + theme + ggtitle(sample)
    image_plot <- if (spatial && side_by_side && !is.null(image)) background + theme else NULL
    panel <- if (is.null(image_plot)) plot else cowplot::plot_grid(image_plot, plot, nrow = 1)
    list(p = panel, features = plot, image = image_plot, domain = app_spot_domain(d$value),
        values = df$value, guide_size = guide_size, warnings = d$warnings, points = sum(keep))
}

# Measure legends outside the map area, wrapping long discrete legends to fit.
app_static_legend <- function(plot, guide_size, values, max_height = 300) {
    measure <- function(grob) {
        if (is.null(grob)) return(c(width = 0, height = 0))
        ceiling(c(width = grid::convertWidth(sum(grob$widths), "inches", valueOnly = TRUE),
            height = grid::convertHeight(sum(grob$heights), "inches", valueOnly = TRUE)) * 96)
    }
    legend <- cowplot::get_legend(plot)
    size <- measure(legend)
    scale <- plot$scales$get_scales("colour")
    if (!is.null(legend) && scale$is_discrete() && size[["height"]] > max_height) {
        keys <- length(unique(c(scale$limits, values)))
        columns <- 1L
        while (size[["height"]] > max_height && columns < keys) {
            columns <- min(keys, max(columns + 1L, ceiling(columns * size[["height"]] / max_height)))
            legend <- cowplot::get_legend(plot + guides(colour = guide_legend(
                ncol = columns, override.aes = list(size = guide_size))))
            size <- measure(legend)
        }
    }
    list(grob = legend, width = size[["width"]], height = size[["height"]])
}

# Arrange fixed physical map viewports; titles and measured legends add space
# outside those viewports rather than taking space away from the spatial data.
app_static_grid <- function(plots, cols, shared_legend, kind) {
    # Grid text measurements need a device, including when a grid is built
    # outside renderPlot(). A null PDF avoids creating an Rplots.pdf file.
    if (grDevices::dev.cur() == 1L) {
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off(), add = TRUE)
    }
    cols <- min(cols, length(plots))
    rows <- ceiling(length(plots) / cols)
    side <- 300
    gap <- 8
    legend_gap <- 12
    padding <- 4
    title_height <- 24
    features <- lapply(plots, `[[`, "features")
    if (shared_legend) {
        scale <- features[[1]]$scales$get_scales("colour")$clone()
        scale$limits <- if (kind == "discrete") {
            sort(unique(unlist(lapply(plots, `[[`, "values"))))
        } else range(unlist(lapply(plots, `[[`, "domain")))
        # Replacing the sample scales is intentional; retain palettes and titles.
        features <- lapply(features, function(p) suppressMessages(p + scale))
    }
    legends <- if (shared_legend) {
        list(app_static_legend(features[[1]], plots[[1]]$guide_size, scale$limits))
    } else lapply(seq_along(plots), function(i) app_static_legend(features[[i]], plots[[i]]$guide_size, plots[[i]]$values))
    legend_width <- max(vapply(legends, `[[`, numeric(1), "width"))
    legend_height <- max(vapply(legends, `[[`, numeric(1), "height"))
    paired <- any(vapply(plots, function(p) !is.null(p$image), logical(1)))
    map_width <- if (paired) 2 * side + gap else side
    block_width <- map_width + if (!shared_legend && legend_width > 0) legend_gap + legend_width else 0
    block_height <- title_height + if (shared_legend) side else max(side, legend_height)
    grid_width <- cols * block_width + (cols - 1) * gap
    grid_height <- rows * block_height + (rows - 1) * gap
    width <- 2 * padding + grid_width + if (shared_legend && legend_width > 0) legend_gap + legend_width else 0
    height <- 2 * padding + if (shared_legend) max(grid_height, title_height + legend_height) else grid_height
    grobs <- list()
    # Positions and sizes are CSS pixels converted to inches at 96 dpi. The
    # device uses this exact canvas in both Shiny and downloaded PDF/PNG files.
    place <- function(grob, x, y, w, h, name) {
        if (is.null(grob)) return(invisible(NULL))
        grobs[[length(grobs) + 1L]] <<- grid::grobTree(grob, name = name,
            vp = grid::viewport(x = grid::unit(x / 96, "inches"),
                y = grid::unit((height - y) / 96, "inches"),
                width = grid::unit(w / 96, "inches"), height = grid::unit(h / 96, "inches"),
                just = c("left", "top"), clip = "on"))
    }
    map_grob <- function(plot) ggplot2::ggplotGrob(plot + labs(title = NULL) +
        theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0)))
    for (i in seq_along(plots)) {
        x <- padding + ((i - 1) %% cols) * (block_width + gap)
        y <- padding + ((i - 1) %/% cols) * (block_height + gap)
        place(grid::textGrob(features[[i]]$labels$title, gp = grid::gpar(fontsize = 12)),
            x, y, map_width, title_height, paste0("title-", i))
        if (paired) {
            image <- plots[[i]]$image
            image <- if (is.null(image)) grid::textGrob("No histology image", gp = grid::gpar(col = "grey50")) else map_grob(image)
            place(image, x, y + title_height, side, side, paste0("map-image-", i))
        }
        feature_x <- x + if (paired) side + gap else 0
        place(map_grob(features[[i]]), feature_x, y + title_height, side, side, paste0("map-feature-", i))
        if (!shared_legend) place(legends[[i]]$grob, x + map_width + legend_gap,
            y + title_height, legend_width, legends[[i]]$height, paste0("legend-", i))
    }
    if (shared_legend) place(legends[[1]]$grob, padding + grid_width + legend_gap,
        padding + title_height, legend_width, legend_height, "legend-shared")
    list(p = grid::grobTree(children = do.call(grid::gList, grobs)),
        width = width, height = height, rows = rows, cols = cols)
}

# Snapshot grid settings on Update so the displayed plot and its download stay consistent.
app_spot_grids_server <- function(input, output, spe, controls, catalog, session) {
    view <- controls$input
    # Grid images are independent of Interactive and never select a pyramid.
    observeEvent(list(input$grid_samples, catalog()), {
        entries <- catalog()[input$grid_samples]
        choices <- app_image_choices(entries, "image")
        old <- isolate(input$grid_imageid)
        selected <- if (length(old) == 1L && old %in% choices) old else app_preferred(choices, "lowres")
        updateSelectInput(session, "grid_imageid", choices = choices, selected = selected)
    }, ignoreNULL = FALSE)
    observeEvent(input$grid_samples, {
        maximum <- max(1L, length(input$grid_samples))
        value <- isolate(input$grid_ncol)
        if (length(value) == 1L && is.finite(value)) value <- min(value, maximum)
        updateNumericInput(session, "grid_ncol", value = value, max = maximum)
    }, ignoreNULL = FALSE)
    grid <- eventReactive(input$grid_update, {
        req(length(input$grid_samples) > 0, view$cluster, length(view$geneid) > 0)
        # Columns determine the occupied layout; there are no empty extra rows.
        cols <- input$grid_ncol
        validate(need(length(cols) == 1L && is.finite(cols) && cols >= 1 &&
            cols <= .Machine$integer.max && cols == floor(cols),
            "Columns must be positive whole numbers."))
        cols <- min(as.integer(cols), length(input$grid_samples))
        validate(need(length(input$grid_pointsize) == 1L && is.finite(input$grid_pointsize) && input$grid_pointsize > 0,
            "Enter a positive point size."))
        # Build all sample panels from one saved set of feature and appearance settings.
        gene <- controls$gene()
        plots <- lapply(input$grid_samples, function(sample) app_static_spatial(
            spe(), sample, view$cluster, gene, as.character(view$cluster_subset), input$grid_kind,
            list(discrete = controls$discrete(), continuous = controls$continuous()), catalog(),
            input$grid_imageid, isTRUE(input$grid_spatial), input$grid_pointsize, view$guidepointsize,
            view$alphalevel, isTRUE(input$grid_side_by_side), fit_to_units = isTRUE(input$grid_fit_to_units)))
        shared_legend <- isTRUE(input$grid_shared_legend)
        layout <- app_static_grid(plots, cols, shared_legend, input$grid_kind)
        c(layout, list(warnings = unique(unlist(lapply(plots, `[[`, "warnings"))),
            # Choose from the points actually drawn in this saved grid. The
            # threshold approximates the PDF / 300 dpi PNG file-size crossover.
            format = if (mean(vapply(plots, `[[`, numeric(1), "points")) < 50000) "pdf" else "png",
            side_by_side = isTRUE(input$grid_spatial) && isTRUE(input$grid_side_by_side),
            fit_to_units = isTRUE(input$grid_fit_to_units),
            shared_legend = shared_legend,
            stem = paste("static_grid", input$grid_kind,
                if (input$grid_kind == "continuous") paste(view$geneid, collapse = "_") else view$cluster,
                paste(input$grid_samples, collapse = "_"), sep = "_")))
    }, ignoreNULL = FALSE)
    output$grid_static <- renderUI({
        settings <- grid()
        div(style = "overflow-x: auto; max-width: 100%;",
            plotOutput("spot_grid", width = paste0(settings$width, "px"), height = settings$height))
    })
    output$spot_grid <- renderPlot({
        grid::grid.newpage()
        grid::grid.draw(grid()$p)
    }, res = 96)
    output$grid_warnings <- renderText(app_warning_text(grid))
    output$downloadPlotGrid <- downloadHandler(
        filename = function() app_filename(grid()$stem, grid()$format),
        content = function(file) {
            settings <- grid()
            # Use saved physical dimensions, independent of browser width.
            width <- settings$width / 96
            height <- settings$height / 96
            if (settings$format == "pdf") {
                grDevices::pdf(file, width = width, height = height, useDingbats = FALSE)
            } else {
                grDevices::png(file, width = width, height = height,
                    units = "in", res = 300, bg = "white")
            }
            on.exit(grDevices::dev.off(), add = TRUE)
            grid::grid.newpage()
            grid::grid.draw(settings$p)
        }
    )
    list(grid = grid)
}
