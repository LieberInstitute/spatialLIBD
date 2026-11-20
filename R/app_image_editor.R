# Build the image editor and manage its session-local image state.
# Share defaults and editing arguments across UI initialization, reset, preview,
# and application to all samples. Expose updated images reactively to other views.

# Build low-resolution editing controls and the preview, reset, and apply actions.
app_image_editor_ui <- function() {
    defaults <- app_image_defaults()
    tabPanel(
        "Edit image",
        selectInput("editImg_imageid", "Low-resolution image to edit", choices = character()),
        helpText("Edits create a low-resolution image variant. Full-resolution image pyramids remain unchanged."),
        helpText(
            "Adjust the colors and appearance of the selected low-resolution image. Click 'Update custom image' to apply the edits, then select 'edited_imaged' under Histology image to use the result in other plots. Most of these image manipulations are explained at",
            HTML(
                "<a href='https://docs.ropensci.org/magick/reference/color.html'>the magick R package documentation</a>."
            )
        ),
        helpText(
            "If you want a uniform colored background, set the brightness to 0 which will make it black, then either proceeed or select the 'negate' checkbox for white, click the 'edit custom image' button, and select the input 'image name' as 'edited image'. Instead of using 'negate' you could use 'transparent (color)' and type 'black' then under 'background (color)' type a valid R color name such as 'purple' or 'lightblue' or a color HEX value such as '#e1eb34'."
        ),
        hr(),
        fluidRow(
            column(
                width = 4,
                selectInput(
                    inputId = "editImg_channel",
                    label = "Select an image channel such as 'Red' or 'Blue'",
                    choices = c(
                        "",
                        magick::channel_types()
                    ),
                    selected = defaults$channel
                ),
                helpText(
                    "Leave this empty if you don't want to select a channel. Note that the definition of channel here is different from a multi-channel image from say VisiumIF."
                ),
                hr(),
                numericInput(
                    "editImg_brightness",
                    label = "Image brightness level",
                    value = defaults$brightness,
                    min = 0,
                    max = 100
                ),
                numericInput(
                    "editImg_saturation",
                    label = "Image saturation level",
                    value = defaults$saturation,
                    min = 0,
                    max = 100
                ),
                numericInput(
                    "editImg_hue",
                    label = "Image hue level",
                    value = defaults$hue,
                    min = 0,
                    max = 200
                ),
                helpText(
                    "Modulate the colors in the image. Brightness and saturation are in percents while hue has a range of 0 to 200. Use 100 for all 3 options for no change."
                )
            ),
            column(
                width = 4,
                checkboxInput(
                    "editImg_enhance",
                    "enhance: attempt to minimize noise",
                    value = defaults$enhance
                ),
                checkboxInput(
                    "editImg_normalize",
                    "normalize: increases contrast by normalizing the pixel values to span the full range of colors",
                    value = defaults$normalize
                ),
                hr(),
                numericInput(
                    "editImg_contrast_sharpen",
                    label = "contrast (sharpen): enhance intensity differences in image",
                    value = defaults$contrast_sharpen,
                    min = -100,
                    max = 100
                ),
                helpText(
                    "Try with 1 to start with."
                ),
                hr(),
                numericInput(
                    "editImg_quantize_max",
                    label = "quantize (max): reduce number of colors in the image",
                    value = defaults$quantize_max,
                    min = 1
                ),
                checkboxInput(
                    "editImg_quantize_dither",
                    "quantize (dither): whether to apply Floyd/Steinberg error diffusion to the image: averages intensities of several neighboring pixels",
                    value = defaults$quantize_dither
                ),
                helpText(
                    "You could try 256 colors or a much small number like 25 or 40."
                )
            ),
            column(
                width = 4,
                checkboxInput(
                    "editImg_equalize",
                    "equalize: whether to use histogram equalization",
                    value = defaults$equalize
                ),
                hr(),
                textInput(
                    "editImg_transparent_color",
                    label = "transparent (color): set pixels approximately matching given color",
                    value = defaults$transparent_color
                ),
                numericInput(
                    "editImg_transparent_fuzz",
                    label = "transparent (fuzz): relative color distance (value between 0 and 100) to be considered similar",
                    value = defaults$transparent_fuzz,
                    min = 0,
                    max = 100
                ),
                helpText(
                    "Type 'purple' and select a fuzz of 25 to start with."
                ),
                textInput(
                    "editImg_background_color",
                    label = "background (color): sets background color",
                    value = defaults$background_color
                ),
                hr(),
                numericInput(
                    "editImg_median_radius",
                    label = "median (radius): replace each pixel with the median color in a circular neighborhood",
                    value = defaults$median_radius,
                    min = 0
                ),
                helpText(
                    "Choose a small radius, like 1 or 2 to start. The higher the value, the longer this computation will take."
                ),
                hr(),
                checkboxInput(
                    "editImg_negate",
                    "negate: whether to negate colors",
                    value = defaults$negate
                )
            )
        ),
        actionButton(
            "editImg_reset_menus",
            label = "Reset menus"
        ),
        helpText(
            "Reset all image editing menus to their default values."
        ),
        hr(),
        downloadButton(
            "downloadPlotEditImg",
            "Download PDF"
        ),
        plotOutput("editImg_plot", height = 600),
        div(class = "app-plot-spacing"),
        actionButton(
            "editImg_update",
            label = "Update custom image"
        ),
        helpText(
            "Click 'Update custom image' to apply these edits to every sample. Select 'edited_imaged' as the background image to use the result in other plots."
        ),
        checkboxInput(
            "editImg_overwrite",
            "Whether to overwrite the 'edited_imaged'",
            value = FALSE
        ),
        helpText(
            "Select if you want to do sequential image manipulations when you have selected as input the 'edited_imaged'."
        )
    )
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
    observeEvent(list(input$sample, catalog()), {
        sources <- catalog()[[input$sample]]$sources
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
        req(input$sample, nzchar(input$editImg_imageid))
        edit(current_spe(), input$sample)$pixels
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
        function() paste("editedImage", input$editImg_imageid, input$sample, sep = "_"),
        function() plot(preview()))
    list(spe = reactive(current_spe()), preview = preview)
}
