# Maintain session-local annotations, one-step undo, and CSV import/export.
# The renderer sends stable keys. Validate them against the current eligible set
# on the server, rather than trusting browser indices or stale hidden units.
app_annotations_server <- function(input, output, session, spe, gene_data, revision) {
    initial <- as.character(spe$ManualAnnotation)
    if (length(initial) != ncol(spe)) initial <- rep("NA", ncol(spe))
    labels <- reactiveVal(initial)
    undo <- reactiveVal(NULL)
    # Restrict annotation actions to the currently visible, eligible units.
    eligible <- function() gene_data()$key
    # Save the affected labels before applying an edit so the action can be undone.
    apply_labels <- function(keys, label) {
        if (!nzchar(trimws(label))) {
            showNotification("Enter an annotation label.", type = "message")
            return(invisible(NULL))
        }
        index <- which(spe$key %in% keys)
        if (!length(index)) return(invisible(NULL))
        current <- labels()
        undo(list(index = index, labels = current[index]))
        current[index] <- label
        labels(current)
    }
    # Reject another sample's events and intersect browser keys with server eligibility.
    event_keys <- function(event) {
        if (is.null(event) || !identical(event$sample, input$sample)) return(character())
        intersect(unlist(event$keys, use.names = FALSE), eligible())
    }
    observeEvent(input$update_manual_ann, {
        keys <- event_keys(input$viewer_selection)
        if (!length(keys)) {
            showNotification("Select visible units before assigning a label.", type = "message")
        } else apply_labels(keys, input$label_manual_ann)
    })
    # Ignore clicks from an older feature payload after filters or values have changed.
    observeEvent(input$viewer_click, {
        if (isTRUE(input$label_click) && isTRUE(input$viewer_click$revision == revision())) {
            apply_labels(event_keys(input$viewer_click), input$label_manual_ann)
        }
    })
    observeEvent(input$undo_manual_ann, {
        previous <- undo()
        if (is.null(previous)) return()
        current <- labels()
        current[previous$index] <- previous$labels
        labels(current)
        undo(NULL)
    })
    output$click <- renderPrint({
        event <- input$viewer_click
        if (is.null(event)) "Click a unit to inspect its ID." else unlist(event$keys)
    })
    # Export sample/unit identities so labels can be restored independently of row order.
    export <- reactive({
        data <- data.frame(sample_id = spe$sample_id, spot_name = colnames(spe),
            ManualAnnotation = labels(), stringsAsFactors = FALSE)
        if (isTRUE(input$dropNA)) data <- data[!is.na(data$ManualAnnotation) & data$ManualAnnotation != "NA", ]
        data
    })
    output$downloadData <- app_csv_download(function() "ManualAnnotation", export, quote = TRUE)
    # Stage all imported labels before publishing them; later matching rows take precedence.
    observeEvent(input$priorGuesses, {
        tryCatch({
            current <- labels()
            matched <- 0L
            for (path in input$priorGuesses$datapath) {
                previous <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
                if (!all(c("sample_id", "spot_name", "ManualAnnotation") %in% names(previous))) {
                    stop("Annotation CSV requires sample_id, spot_name, and ManualAnnotation columns.")
                }
                previous <- previous[!is.na(previous$ManualAnnotation) & previous$ManualAnnotation != "NA", ]
                index <- match(paste(previous$sample_id, previous$spot_name, sep = "\034"),
                    paste(spe$sample_id, colnames(spe), sep = "\034"))
                missing <- is.na(index)
                index[missing] <- match(paste0(previous$spot_name[missing], "_", previous$sample_id[missing]), spe$key)
                good <- which(!is.na(index))
                # Sequential assignment also gives the last row precedence.
                for (j in good) current[index[j]] <- previous$ManualAnnotation[j]
                matched <- matched + length(good)
            }
            if (!matched) stop("No annotation rows match these sample and unit IDs.")
            undo(list(index = seq_len(ncol(spe)), labels = labels()))
            labels(current)
        }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    labels
}
