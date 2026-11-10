# Build linked spatial/embedding panels and the standalone interactive gene view.
# Reuse expression preparation, compose Plotly panels with shared spot keys and
# backgrounds, and expose thresholded expression data to annotation handling.

app_cluster_interactive_ui <- function() {
    tabPanel(
        "Clusters (interactive)",
        plotlyOutput(
            "histology_plotly",
            width = "1440px",
            height = "1200px"
        ),
        helpText(
            "This is a 2 by 2 plotting area with the top row showing the spatially-resolved data on top of the histology images. The bottom row shows the data on the reduced dimensions. The left column shows the selected continuous variable (typically a gene) while the right column shows the selected discreate variable (typically cluster annotations)."
        ),
        textInput(
            "label_manual_ann",
            "Manual annotation label",
            "Your Guess"
        ),
        checkboxInput(
            "label_click",
            "Enable spot manual annotations by clicking on individual spots (double left-mouse click).",
            value = FALSE
        ),
        verbatimTextOutput("click"),
        actionButton(
            "update_manual_ann",
            "Label selected points (from lasso) with manual annotation"
        ),
        helpText(
            "Select points (lasso) to label them with a manual annotation."
        ),
        div(class = "app-plot-spacing"),
    )
}

app_gene_interactive_ui <- function() {
    tabPanel(
        "Gene (interactive)",
        uiOutput("gene_plotly_cluster_subset_ui"),
        plotlyOutput(
            "gene_plotly",
            width = "600px",
            height = "600px"
        ),
        textInput(
            "label_manual_ann_gene",
            "Manual annotation label",
            "Your Guess"
        ),
        checkboxInput(
            "label_click_gene",
            "Enable spot manual annotations by clicking on individual spots (double left-mouse click).",
            value = FALSE
        ),
        verbatimTextOutput("click_gene"),
        actionButton(
            "update_manual_ann_gene",
            "Label selected points (from lasso) with manual annotation"
        ),
        helpText(
            "Select points to label them with a manual annotation."
        ),
        helpText(
            "Note that only spots passing the minimum count value will be updated."
        ),
        div(class = "app-plot-spacing"),
    )
}

app_spot_interactive_server <- function(input, output, spe, colors, settings) {
    gene_plot <- reactive({
        req(input$sample, input$cluster)
        selected <- input$gene_plotly_cluster_subset
        req(length(selected) > 0)
        subset <- spe()[, as.character(spe()[[input$cluster]]) %in% selected]
        req(any(subset$sample_id == input$sample))
        do.call(vis_gene, c(
            list(spe = subset, sampleid = input$sample, spatial = FALSE),
            settings(), app_gene_settings(input, colors$continuous)
        ))
    })
    gene_data <- reactive(gene_plot()$data[, c("key", "COUNT"), drop = FALSE])
    output$gene_plotly_cluster_subset_ui <- renderUI({
        req(input$cluster)
        labels <- unique(spe()[[input$cluster]])
        checkboxGroupInput("gene_plotly_cluster_subset", "Select clusters to show in this plot",
            choices = sort(labels), selected = labels, inline = TRUE
        )
    })
    output$gene_plotly <- renderPlotly({
        plot <- gene_plot() + geom_point(
            shape = 21, size = input$pointsize,
            stroke = 0, alpha = input$alphalevel
        )
        image <- app_background_image(spe(), input$sample, settings())
        suppressMessages(suppressWarnings(toWebGL(
            app_plotly_spatial(plot, image, "plotly_gene", width = 600, height = 600)
        )))
    })
    output$histology_plotly <- renderPlotly({
        req(input$sample, input$cluster)
        app_linked_spot_plot(
            spe(), input$sample, input$cluster, input$reduced_name,
            colors$discrete(), settings(), app_gene_settings(input, colors$continuous)
        )
    })
    list(gene_data = gene_data)
}

app_background_image <- function(spe, sample, settings) {
    if (settings$datatype == "Xenium") {
        return(NULL)
    }
    image <- SpatialExperiment::imgRaster(spe, sample_id = sample, image_id = settings$image_id)
    if (isTRUE(settings$auto_crop)) {
        frame <- frame_limits(spe, sampleid = sample, image_id = settings$image_id)
        image <- image[frame$y_min:frame$y_max, frame$x_min:frame$x_max]
    }
    image
}

# The same image positioning is used by standalone and linked spatial panels.
app_plotly_spatial <- function(plot, image, source, width = NULL, height = NULL) {
    background <- if (!is.null(image)) {
        list(list(
            source = raster2uri(image), layer = "below", xanchor = "left", yanchor = "bottom",
            xref = "x", yref = "y", sizing = "stretch", x = 0, y = -nrow(image),
            sizex = ncol(image), sizey = nrow(image), opacity = 0.8
        ))
    } else {
        NULL
    }
    layout(ggplotly(plot,
        width = width, height = height, source = source,
        tooltip = c("fill", "key")
    ), images = background, dragmode = "lasso")
}

app_interactive_title <- function(spe, sample, gene) {
    if (length(gene$geneid) == 1) {
        assay <- if (gene$geneid %in% colnames(colData(spe))) "" else paste0(gene$assayname, " ")
        return(paste0(sample, " ", gene$geneid, " ", assay, "min > ", gene$minCount))
    }
    label <- switch(gene$multi_gene_method,
        z_score = "Z-score",
        sparsity = "Prop. nonzero",
        pca = "PC1"
    )
    paste(sample, label, "min >", gene$minCount)
}

app_linked_spot_plot <- function(spe, sample, cluster, reduced_name, colors, settings, gene) {
    COUNT <- key <- NULL
    # Reuse the public expression preparation so assay selection, aggregation,
    # percentile capping, missing values, and stitched exclusions match static plots.
    expression <- do.call(vis_gene, c(
        list(spe = spe, sampleid = sample, spatial = FALSE),
        settings, gene
    ))
    data <- expression$data
    data <- data[!is.na(data$COUNT), , drop = FALSE]
    req(nrow(data) > 0)
    subset <- spe[, spe$sample_id == sample]
    if (settings$datatype == "Visium" && settings$is_stitched) {
        subset <- prep_stitched_data(subset, settings$point_size, settings$image_id)$spe
    }
    point_size <- settings$point_size
    if (settings$datatype == "Visium" && settings$is_stitched) {
        point_size <- prep_stitched_data(
            spe[, spe$sample_id == sample],
            point_size, settings$image_id
        )$point_size
    }
    has_dims <- length(reduced_name) == 1 && nzchar(reduced_name)
    if (has_dims) {
        dimensions <- reducedDim(subset, reduced_name)[match(data$key, subset$key), , drop = FALSE]
        has_dims <- ncol(dimensions) >= 2
        if (has_dims) {
            colnames(dimensions) <- paste(reduced_name, "dim", seq_len(ncol(dimensions)))
            data <- cbind(data, dimensions)
        }
    }
    linked <- highlight_key(data, ~key)
    title <- app_interactive_title(spe, sample, gene)
    common <- list(
        spe = subset, d = linked, sampleid = sample,
        alpha = settings$alpha, point_size = point_size
    )
    if (settings$datatype == "Visium") {
        spatial <- list(spatial = FALSE, image_id = settings$image_id, auto_crop = settings$auto_crop)
        clusters <- do.call(vis_clus_p, c(common, spatial, list(
            clustervar = cluster, colors = get_colors(colors, data[[cluster]]), title = title
        )))
        expression <- do.call(vis_gene_p, c(common, spatial, list(title = "", cont_colors = gene$cont_colors)))
    } else {
        clusters <- do.call(vis_clus_c, c(common, list(
            clustervar = cluster, colors = get_colors(colors, data[[cluster]]), title = title
        )))
        expression <- do.call(vis_gene_c, c(common, list(title = "", cont_colors = gene$cont_colors)))
    }
    expression <- expression + geom_point(shape = 21, size = point_size, stroke = 0, alpha = settings$alpha)
    # Plotly supplies keys from highlight_key() for every layer. Keeping the
    # workhorse's explicit mapping as well duplicates that aesthetic in ggplot2.
    clusters$mapping$key <- expression$mapping$key <- NULL
    dimension_theme <- theme_bw(base_size = 20) + theme(
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
    )
    if (has_dims) {
        axes <- list(x = rlang::sym(colnames(dimensions)[1]), y = rlang::sym(colnames(dimensions)[2]))
        dim_clusters <- ggplot(linked, aes(
            x = !!axes$x, y = !!axes$y,
            fill = factor(!!rlang::sym(cluster))
        )) +
            geom_point(shape = 21, size = point_size, stroke = 0) +
            scale_fill_manual(values = get_colors(colors, spe[[cluster]][spe$sample_id == sample])) +
            guides(fill = "none")
        dim_expression <- ggplot(linked, aes(x = !!axes$x, y = !!axes$y, fill = COUNT, color = COUNT)) +
            geom_point(shape = 21, size = point_size, stroke = 0)
    } else {
        dim_clusters <- dim_expression <- ggplot(linked)
    }
    dim_clusters <- dim_clusters + ggtitle("") + dimension_theme
    dim_expression <- dim_expression + ggtitle("") + dimension_theme +
        scale_fill_gradientn(colors = gene$cont_colors, na.value = "#CCCCCC40", guide = "none") +
        scale_color_gradientn(colors = gene$cont_colors, na.value = "#CCCCCC40", guide = "none")

    source <- "plotly_histology"
    image <- app_background_image(spe, sample, settings)
    cluster_widget <- app_plotly_spatial(clusters, image, source, width = 1440, height = 1200)
    gene_widget <- app_plotly_spatial(expression, image, source)
    if (!has_dims) {
        # Without an embedding there are only two spatial panels to link.
        merged <- subplot(gene_widget, cluster_widget,
            nrows = 1,
            shareX = TRUE, shareY = TRUE, which_layout = 2
        )
        return(suppressMessages(suppressWarnings(toWebGL(
            highlight(merged, on = "plotly_selected", off = "plotly_deselect")
        ))))
    }
    dim_widget <- ggplotly(dim_clusters, source = source, tooltip = c("fill", "key"))
    dim_gene_widget <- ggplotly(dim_expression, source = source, tooltip = c("fill", "key"))
    merged <- layout(subplot(
        subplot(gene_widget, cluster_widget, nrows = 1, shareX = TRUE, shareY = TRUE, which_layout = 2),
        style(subplot(dim_gene_widget, dim_widget,
            nrows = 1, shareX = TRUE, shareY = TRUE,
            which_layout = 2
        ), showlegend = FALSE),
        nrows = 2, shareX = FALSE, shareY = FALSE, which_layout = 1
    ), legend = list(tracegroupgap = 0, x = 1.2))
    merged$x$layout$xaxis3$title <- merged$x$layout$xaxis4$title <- dim_widget$x$layout$xaxis$title
    merged$x$layout$yaxis2$title <- dim_widget$x$layout$yaxis$title
    suppressMessages(suppressWarnings(toWebGL(
        highlight(merged, on = "plotly_selected", off = "plotly_deselect")
    )))
}
