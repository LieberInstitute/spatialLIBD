# Keep editing settings beside the preview; detailed guidance lives in info icons.
app_image_editor_ui <- function(spe) {
    defaults <- app_image_defaults()
    label <- function(name, title, ...) app_control_label(paste0("editImg_", name), title, ...)
    tabPanel("Edit image", value = "edit_image", sidebarLayout(
        sidebarPanel(width = 3,
            shinyWidgets::pickerInput("editImg_sample", label("sample", "Preview sample",
                "Choose the sample to preview. Apply edits to all samples processes every sample, independently of this selection."),
                choices = unique(spe$sample_id), options = shinyWidgets::pickerOptions(liveSearch = TRUE)),
            selectInput("editImg_imageid", label("imageid", "Low-resolution image",
                tags$p("Choose the low-resolution image to edit. The preview updates immediately. Applying edits creates an edited_imaged variant for every sample; full-resolution pyramids remain unchanged."),
                tags$p("Adjustment details: ", tags$a(href = "https://docs.ropensci.org/magick/reference/color.html",
                    target = "_blank", rel = "noopener", "magick image editing documentation"), ".")), choices = character()),
            numericInput("editImg_brightness", label("brightness", "Brightness (%)",
                "100 leaves brightness unchanged; 0 makes the image black. For a white background, use 0 and enable Invert colors."), defaults$brightness, min = 0, max = 100),
            numericInput("editImg_saturation", label("saturation", "Saturation (%)",
                "100 leaves saturation unchanged; 0 removes color."), defaults$saturation, min = 0, max = 100),
            numericInput("editImg_hue", label("hue", "Hue",
                "Shift image hues on a scale from 0 to 200. The default of 100 leaves hues unchanged."), defaults$hue, min = 0, max = 200),
            app_control_section("Channels and contrast",
                selectInput("editImg_channel", label("channel", "Channel",
                    "Choose a color channel, such as Red or Blue, or leave blank to use all channels. These are image color channels, not separate experimental channels such as VisiumIF markers."),
                    choices = c("", magick::channel_types()), selected = defaults$channel),
                checkboxInput("editImg_enhance", label("enhance", "Reduce noise",
                    "Apply image enhancement to attempt to reduce noise."), defaults$enhance),
                checkboxInput("editImg_normalize", label("normalize", "Normalize contrast",
                    "Stretch pixel values across the full color range to increase contrast."), defaults$normalize),
                numericInput("editImg_contrast_sharpen", label("contrast_sharpen", "Contrast adjustment",
                    "Enhance intensity differences. Leave blank to skip this adjustment; try 1 to start."), defaults$contrast_sharpen, min = -100, max = 100),
                checkboxInput("editImg_equalize", label("equalize", "Equalize histogram",
                    "Redistribute intensities using histogram equalization to adjust contrast."), defaults$equalize)),
            app_control_section("Colors and smoothing",
                numericInput("editImg_quantize_max", label("quantize_max", "Maximum colors",
                    "Reduce the number of colors. Leave blank to retain the current colors; try 256, or a smaller value such as 25 or 40."), defaults$quantize_max, min = 1),
                checkboxInput("editImg_quantize_dither", label("quantize_dither", "Dither reduced colors",
                    "When reducing colors, use Floyd–Steinberg error diffusion to spread quantization error across neighboring pixels."), defaults$quantize_dither),
                textInput("editImg_transparent_color", label("transparent_color", "Transparent color",
                    "Make pixels matching this R color name or hex value transparent. Leave blank to skip. Color tolerance includes nearby colors; for example, try purple with a tolerance of 25."), defaults$transparent_color),
                numericInput("editImg_transparent_fuzz", label("transparent_fuzz", "Color tolerance (%)",
                    "Set how similar a pixel must be to Transparent color. 0 requires an exact match; larger values include more nearby colors."), defaults$transparent_fuzz, min = 0, max = 100),
                textInput("editImg_background_color", label("background_color", "Background color",
                    "Replace transparency with an R color name or hex value, such as lightblue or #e1eb34. To create a uniform background, set brightness to 0, make black transparent, then choose this color. Leave blank to skip."), defaults$background_color),
                numericInput("editImg_median_radius", label("median_radius", "Median filter radius",
                    "Replace each pixel with the median color in a circular neighborhood. Leave blank to skip; start with 1 or 2. Larger radii take longer to compute."), defaults$median_radius, min = 0),
                checkboxInput("editImg_negate", label("negate", "Invert colors",
                    "Invert the image colors; for example, black becomes white."), defaults$negate)),
            tags$hr(), actionButton("editImg_reset_menus", "Reset adjustments"),
            checkboxInput("editImg_overwrite", label("overwrite", "Replace existing edited images",
                "Allow applying edits to replace existing edited_imaged variants. To make sequential edits, select edited_imaged as the input image and enable replacement."), FALSE),
            actionButton("editImg_update", "Apply edits to all samples"),
            downloadButton("downloadPlotEditImg", "Download preview PDF")),
        mainPanel(width = 9, plotOutput("editImg_plot", height = 600), div(class = "app-plot-spacing"))))
}

# UI creation, reset, preview, and apply share these defaults/argument names.
app_image_defaults <- function() {
    list(
        channel = "", brightness = 100, saturation = 100, hue = 100,
        enhance = FALSE, normalize = FALSE, contrast_sharpen = NA_real_,
        quantize_max = NA_real_, quantize_dither = TRUE, equalize = FALSE,
        transparent_color = NA_character_, transparent_fuzz = 0,
        background_color = NA_character_, median_radius = NA_real_, negate = FALSE
    )
}

# Collect the current editor inputs under the argument names expected by img_edit().
app_image_arguments <- function(input) {
    names <- names(app_image_defaults())
    setNames(lapply(names, function(name) input[[paste0("editImg_", name)]]), names)
}

# Preview low-resolution edits and publish registered image variants within this session.
app_image_editor_server <- function(input, output, session, initial_spe, datatype) {
    current_spe <- reactiveVal(initial_spe)
    catalog <- reactive(app_image_catalog(current_spe()))
    # Editing is limited to low-resolution images available for the current sample.
    observeEvent(list(input$editImg_sample, catalog()), {
        sources <- catalog()[[input$editImg_sample]]$sources
        choices <- names(Filter(function(s) s$type == "image", sources))
        old <- isolate(input$editImg_imageid)
        selected <- if (length(old) == 1 && old %in% choices) old else app_preferred(choices, "lowres")
        updateSelectInput(session, "editImg_imageid", choices = choices, selected = selected)
    })
    # Edit a small raster while retaining the scene registration for the resulting variant.
    edit <- function(spe, sample) {
        source <- app_image_source(app_image_catalog(spe), sample, input$editImg_imageid)
        if (is.null(source) || source$type != "image") stop("Select a low-resolution image to edit.")
        raster <- app_image_raster(spe, sample, source)
        # img_edit only consumes a raster, so provide one small image without
        # subsetting the SFE or exposing the full-resolution image handles.
        tmp <- spe
        imgData(tmp) <- S4Vectors::DataFrame(sample_id = sample, image_id = "app_edit",
            data = I(list(SpatialExperiment::SpatialImage(raster$image))), scaleFactor = 1)
        pixels <- as.raster(do.call(img_edit, c(list(spe = tmp, sampleid = sample,
            image_id = "app_edit"), app_image_arguments(input))))
        list(pixels = pixels, scale = source$scale_factor, extent = raster$extent,
            y_direction = if (app_spot_y_down(spe, source)) "down" else "up")
    }
    # Preview changes immediately; applying them to all samples requires an explicit action.
    preview <- reactive({
        req(input$editImg_sample, nzchar(input$editImg_imageid))
        edit(current_spe(), input$editImg_sample)$pixels
    })
    observeEvent(input$editImg_update, {
        tryCatch({
            spe <- current_spe()
            images <- imgData(spe)
            if (any(images$image_id == "edited_imaged") && !isTRUE(input$editImg_overwrite)) {
                stop("Enable overwrite to replace the existing edited image.")
            }
            # Build all results before changing session state.
            samples <- unique(spe$sample_id)
            edits <- setNames(lapply(samples, function(sample) edit(spe, sample)), samples)
            rows <- lapply(samples, function(sample) {
                image <- edits[[sample]]
                S4Vectors::DataFrame(sample_id = sample, image_id = "edited_imaged",
                    data = I(list(SpatialExperiment::SpatialImage(image$pixels))),
                    scaleFactor = if (is.null(image$scale)) 1 else image$scale)
            })
            images <- images[images$image_id != "edited_imaged", ]
            imgData(spe) <- do.call(rbind, c(list(images), rows))
            # Keep the exact scene registration, including native SFE image
            # extents, when the edited pixels become a separate image source.
            registered <- S4Vectors::metadata(spe)$viewer_images
            for (sample in samples) {
                at <- which(vapply(registered, function(x) identical(as.character(x$sample_id), sample), logical(1)))
                if (!length(at)) {
                    at <- length(registered) + 1L
                    registered[[at]] <- list(sample_id = sample,
                        default_image_id = catalog()[[sample]]$default, sources = list())
                }
                entry <- registered[[at[1]]]
                entry$sources <- Filter(function(s) s$image_id != "edited_imaged", entry$sources)
                image <- edits[[sample]]
                entry$sources[[length(entry$sources) + 1L]] <- list(image_id = "edited_imaged",
                    type = "image", scale_factor = image$scale, extent = image$extent,
                    y_direction = image$y_direction)
                registered[[at[1]]] <- entry
            }
            S4Vectors::metadata(spe)$viewer_images <- registered
            revision <- S4Vectors::metadata(spe)$app_image_revision
            S4Vectors::metadata(spe)$app_image_revision <- if (is.null(revision)) 1L else revision + 1L
            current_spe(spe)
            showNotification("Edited low-resolution images are ready. Select edited_imaged to display them.")
        }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })
    # Reset each widget through its matching Shiny update API using the shared defaults.
    observeEvent(input$editImg_reset_menus, {
        for (name in names(app_image_defaults())) {
            value <- app_image_defaults()[[name]]
            id <- paste0("editImg_", name)
            if (name == "channel") updateSelectInput(session, id, selected = value)
            else if (is.logical(value)) updateCheckboxInput(session, id, value = value)
            else if (is.character(value)) updateTextInput(session, id, value = value)
            else updateNumericInput(session, id, value = value)
        }
    })
    output$editImg_plot <- renderPlot(plot(preview()), width = 600, height = "auto")
    output$downloadPlotEditImg <- app_pdf_download(
        function() paste("editedImage", input$editImg_imageid, input$editImg_sample, sep = "_"),
        function() plot(preview()))
    list(spe = reactive(current_spe()), preview = preview)
}
