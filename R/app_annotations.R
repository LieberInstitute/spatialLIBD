# Own the session's manual spot labels and handle click/lasso annotation actions.
# Import and export labels by stable spot identity. Expression-tab annotation uses
# the current expression subset and threshold mask supplied by the interactive view.

app_annotations_server <- function(input, output, session, spe, gene_data) {
    labels <- reactiveVal(rep("NA", ncol(spe)))
    apply_labels <- function(keys, label) {
        current <- labels()
        current[spe$key %in% keys] <- label
        labels(current)
    }
    selection <- function(event, source, expression = FALSE) {
        data <- plotly::event_data(event, source = source, session = session, priority = "event")
        if (is.null(data)) {
            return(character())
        }
        keys <- data$key
        if (expression) {
            counts <- gene_data()
            keys <- intersect(keys, counts$key[!is.na(counts$COUNT)])
        }
        keys
    }
    observeEvent(input$update_manual_ann, {
        apply_labels(selection("plotly_selected", "plotly_histology"), input$label_manual_ann)
    })
    observeEvent(input$update_manual_ann_gene, {
        apply_labels(selection("plotly_selected", "plotly_gene", TRUE), input$label_manual_ann_gene)
    })
    # Mutations are observers, not side effects of rendering the click text.
    observeEvent(plotly::event_data("plotly_click",
        source = "plotly_histology",
        session = session, priority = "event"
    ), {
        if (isTRUE(input$label_click)) {
            apply_labels(selection("plotly_click", "plotly_histology"), input$label_manual_ann)
        }
    })
    observeEvent(plotly::event_data("plotly_click",
        source = "plotly_gene",
        session = session, priority = "event"
    ), {
        if (isTRUE(input$label_click_gene)) {
            apply_labels(selection("plotly_click", "plotly_gene", TRUE), input$label_manual_ann_gene)
        }
    })
    click_text <- function(source) {
        renderPrint({
            event <- plotly::event_data("plotly_click",
                source = source,
                session = session, priority = "event"
            )
            if (is.null(event)) "Clicked spot IDs appear here (double-click to clear)." else event$key
        })
    }
    output$click <- click_text("plotly_histology")
    output$click_gene <- click_text("plotly_gene")
    export <- reactive({
        data <- data.frame(
            sample_id = spe$sample_id, spot_name = colnames(spe),
            ManualAnnotation = labels(), stringsAsFactors = FALSE
        )
        if (isTRUE(input$dropNA)) data <- data[!is.na(data$ManualAnnotation) & data$ManualAnnotation != "NA", ]
        data
    })
    output$downloadData <- app_csv_download(function() "ManualAnnotation", export, quote = TRUE)
    observeEvent(input$priorGuesses, {
        previous <- utils::read.csv(input$priorGuesses$datapath,
            stringsAsFactors = FALSE, na.strings = ""
        )
        req(all(c("sample_id", "spot_name", "ManualAnnotation") %in% names(previous)))
        previous <- previous[!is.na(previous$ManualAnnotation) & previous$ManualAnnotation != "NA", ]
        keys <- paste0(previous$spot_name, "_", previous$sample_id)
        index <- match(keys, spe$key)
        if (all(is.na(index))) {
            index <- match(paste0(previous$sample_id, "_", previous$spot_name), spe$key)
        }
        if (all(is.na(index))) stop("Cannot use previous manual annotations.", call. = FALSE)
        current <- labels()
        current[index[!is.na(index)]] <- previous$ManualAnnotation[!is.na(index)]
        labels(current)
    })
    labels
}
