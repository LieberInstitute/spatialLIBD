# Provide small shared helpers for plot composition, warnings, table presentation,
# downloads, example gene sets, and common plotting arguments. Feature files supply
# their own data and settings; scientific calculations remain in the public helpers.

# Keep labels compact while making their explanations available on demand.
app_control_label <- function(id, label, ...) {
    help_id <- paste0(id, "-help")
    tagList(label, " ",
        tags$button(type = "button", class = "sl-control-info",
            `data-help-id` = help_id, `data-help-title` = label,
            `aria-label` = paste("About", label), `aria-expanded` = "false",
            icon("circle-info")),
        tags$span(id = help_id, style = "display: none;", ...))
}

# Give expandable settings groups a consistent button-like summary and state cue.
app_control_section <- function(title, ...) {
    tags$details(class = "sl-appearance", tags$summary(tags$span(title),
        tags$span(class = "sl-appearance-show", "Show settings"),
        tags$span(class = "sl-appearance-hide", "Hide settings")), ...)
}

# Choose a usable default, falling back to the first available choice or none.
app_preferred <- function(choices, preferred) {
    if (length(preferred) == 1 && preferred %in% choices) preferred else if (length(choices)) choices[[1]] else ""
}

# Give downloads a timestamped name without spaces or path separators.
app_filename <- function(stem, extension) {
    gsub("[ :/]", "_", paste0("spatialLIBD_", stem, "_", Sys.time(), ".", extension))
}

# Wrap a drawing callback in a PDF download and always close its device.
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

# Export the current data with consistent naming and caller-selected CSV options.
app_csv_download <- function(stem, data, row_names = FALSE, quote = FALSE) {
    downloadHandler(
        filename = function() app_filename(stem(), "csv"),
        content = function(file) {
            utils::write.csv(data(), file, row.names = row_names, quote = quote)
        }
    )
}

# Render reactive results with shared filtering, pagination, and optional sorting.
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

# Return a calculation and its distinct warnings for display beside the result.
app_capture_warnings <- function(code) {
    warnings <- character()
    value <- withCallingHandlers(code, warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(p = value, warnings = unique(warnings))
}

# Format captured warnings without duplicating errors already shown by the plot.
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

# Separate the image and point layers of a spatial plot into adjacent panels.
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

# Map model tests to domain colors, pairwise midpoints, or neutral ANOVA grey.
app_model_colors <- function(sce, group_var, groups, model_type) {
    if (model_type == "anova") {
        return(setNames(rep("grey60", length(groups)), groups))
    }
    colors <- app_layer_colors(sce, group_var)
    if (model_type != "pairwise") return(get_colors(colors, groups))

    # Resolve domain colors before assigning colors to comparisons. Keep explicit
    # comparison colors, and use grey when either domain has no supplied color.
    # Preserve support for short LIBD labels such as L1 and L2.
    short_colors <- colors
    names(short_colors) <- gsub("ayer", "", names(short_colors))
    result <- setNames(rep("grey60", length(groups)), groups)
    for (test in groups) {
        if (test %in% names(colors) && !is.na(colors[[test]])) {
            result[test] <- colors[[test]]
            next
        }
        domains <- strsplit(test, "-", fixed = TRUE)[[1]]
        if (length(domains) != 2L) next
        domain_colors <- colors[domains]
        missing <- is.na(domain_colors)
        domain_colors[missing] <- short_colors[domains[missing]]
        if (!anyNA(domain_colors)) {
            result[test] <- scales::colour_ramp(domain_colors)(0.5)
        }
    }
    result
}

# Build the default SFARI gene sets from the bundled gene annotations.
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

# Use uploaded gene-set columns when present, otherwise the bundled examples.
app_read_gene_sets <- function(upload) {
    if (is.null(upload)) {
        return(app_example_gene_sets())
    }
    utils::read.csv(upload$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE, row.names = NULL
    )
}

# Shared feature settings for the interactive viewer and static exports.
app_gene_settings <- function(input, colors) {
    req(length(input$geneid) > 0)
    list(
        geneid = input$geneid, multi_gene_method = input$multi_gene_method,
        assayname = input$assayname, minCount = input$minCount,
        cap_percentile = input$cap_percentile, cont_colors = colors()
    )
}
