# Build the image editor and manage its session-local image state.
# Share defaults and editing arguments across UI initialization, reset, preview,
# and application to all samples. Expose updated images reactively to other views.

app_image_editor_ui <- function() {
    defaults <- app_image_defaults()
    tabPanel(
        "Edit image",
        helpText(
            "Edit the selected image by manipulating the colors and apperance, which can be useful when inspecting the selected image from the left menu ('image name'). Once you have a set of edits you like, click the 'update custom image' button below to save your edits. Next, select on the left menu ('image name') the 'edited_imaged' option to use your new image as the background image in the rest of the visualizations. Most of these image manipulations are explained at",
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
        plotOutput("editImg_plot"),
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

app_image_arguments <- function(input) {
    names <- names(app_image_defaults())
    setNames(lapply(names, function(name) input[[paste0("editImg_", name)]]), names)
}

app_image_editor_server <- function(input, output, session, initial_spe, datatype) {
    current_spe <- reactiveVal(initial_spe)
    preview <- reactive({
        req(datatype != "Xenium", input$sample, input$imageid)
        as.raster(do.call(img_edit, c(list(
            spe = current_spe(), sampleid = input$sample,
            image_id = input$imageid
        ), app_image_arguments(input))))
    })
    if (datatype != "Xenium") {
        observeEvent(input$editImg_update, {
            current_spe(do.call(img_update_all, c(list(
                spe = current_spe(), image_id = input$imageid,
                new_image_id = "edited_imaged", overwrite = isTRUE(input$editImg_overwrite)
            ), app_image_arguments(input))))
        })
        observeEvent(input$editImg_reset_menus, {
            for (name in names(app_image_defaults())) {
                value <- app_image_defaults()[[name]]
                id <- paste0("editImg_", name)
                if (name == "channel") {
                    updateSelectInput(session, id, selected = value)
                } else if (is.logical(value)) {
                    updateCheckboxInput(session, id, value = value)
                } else if (is.character(value)) {
                    updateTextInput(session, id, value = value)
                } else {
                    updateNumericInput(session, id, value = value)
                }
            }
        })
        output$editImg_plot <- renderPlot(plot(preview()), width = 600, height = 600)
        output$downloadPlotEditImg <- app_pdf_download(
            function() paste("editedImage", input$imageid, input$sample, sep = "_"),
            function() plot(preview())
        )
    }
    list(spe = reactive(current_spe()), preview = preview)
}
