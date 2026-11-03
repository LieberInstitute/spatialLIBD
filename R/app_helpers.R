# Small app-only helpers. Scientific calculations remain in the public helpers.
app_preferred <- function(choices, preferred) {
    if (length(preferred) == 1 && preferred %in% choices) preferred else if (length(choices)) choices[[1]] else ""
}

app_filename <- function(stem, extension) {
    gsub("[ :/]", "_", paste0("spatialLIBD_", stem, "_", Sys.time(), ".", extension))
}

app_pdf_download <- function(stem, draw, width = function() 8, height = function() 8) {
    downloadHandler(
        filename = function() app_filename(stem(), "pdf"),
        content = function(file) {
            grDevices::pdf(file, width = width(), height = height(), useDingbats = FALSE)
            on.exit(grDevices::dev.off(), add = TRUE)
            draw()
        }
    )
}

app_csv_download <- function(stem, data, row_names = FALSE, quote = FALSE) {
    downloadHandler(
        filename = function() app_filename(stem(), "csv"),
        content = function(file) {
            utils::write.csv(data(), file, row.names = row_names, quote = quote)
        }
    )
}

app_table <- function(data, row_names = FALSE, direction = NULL) {
    options <- list(
        columnDefs = list(list(className = "dt-center", targets = 1)),
        pageLength = 10, lengthMenu = c(5, 10, 25, 50, 100)
    )
    if (!is.null(direction)) options$order <- list(list(0, direction))
    DT::renderDT(data(),
        style = "bootstrap", rownames = row_names,
        filter = "top", options = options
    )
}

app_capture_warnings <- function(code) {
    warnings <- character()
    value <- withCallingHandlers(code, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(p = value, warnings = unique(warnings))
}

app_warning_text <- function(result) {
    # The plot itself displays errors; the companion warning output needn't repeat them.
    tryCatch(
        {
            warnings <- result()$warnings
            if (length(warnings)) paste("Warning:", paste(warnings, collapse = "; ")) else ""
        },
        error = function(e) ""
    )
}

app_side_by_side <- function(plot, enabled, datatype) {
    if (!enabled || datatype == "Xenium") {
        return(plot)
    }
    background <- plot
    background$layers[[2]] <- NULL
    points <- plot
    points$layers[[1]] <- NULL
    cowplot::plot_grid(background, points + theme(legend.position = "none"), nrow = 1)
}

app_model_colors <- function(sce, group_var, groups) {
    colors <- sce[[paste0(group_var, "_colors")]]
    if (is.null(colors)) colors <- spatialLIBD::libd_layer_colors
    get_colors(colors, groups)
}

app_example_gene_sets <- function() {
    sfari <- utils::read.csv(system.file("extdata",
        "SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv",
        package = "spatialLIBD"
    ))
    list(
        Gene_SFARI_all = sfari$ensembl.id,
        Gene_SFARI_high = sfari$ensembl.id[sfari$gene.score < 3],
        Gene_SFARI_syndromic = sfari$ensembl.id[sfari$syndromic == 1]
    )
}

app_read_gene_sets <- function(upload) {
    if (is.null(upload)) {
        return(app_example_gene_sets())
    }
    utils::read.csv(upload$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE, row.names = NULL
    )
}

# One source of arguments for static plots, grids, and interactive preparation.
app_spot_settings <- function(input, is_stitched, datatype) {
    list(
        image_id = input$imageid, alpha = input$alphalevel,
        point_size = input$pointsize, auto_crop = input$auto_crop && !is_stitched,
        is_stitched = is_stitched, datatype = datatype
    )
}

app_gene_settings <- function(input, colors) {
    req(length(input$geneid) > 0)
    list(
        geneid = input$geneid, multi_gene_method = input$multi_gene_method,
        assayname = input$assayname, minCount = input$minCount,
        cap_percentile = input$cap_percentile, cont_colors = colors()
    )
}
