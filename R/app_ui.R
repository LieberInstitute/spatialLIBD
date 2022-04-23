#' @import shiny
#' @importFrom shinyWidgets pickerInput pickerOptions updatePickerInput
#' @import SingleCellExperiment
#' @importFrom DT DTOutput
#' @importFrom SummarizedExperiment assays
app_ui <- function() {
    ## Get options
    spe <- golem::get_golem_options("spe")
    docs_path <- golem::get_golem_options("docs_path")
    title <- golem::get_golem_options("title")
    spe_discrete_vars <-
        golem::get_golem_options("spe_discrete_vars")
    default_cluster <- golem::get_golem_options("default_cluster")
    sce_layer <- golem::get_golem_options("sce_layer")
    modeling_results <- golem::get_golem_options("modeling_results")
    sig_genes <- golem::get_golem_options("sig_genes")

    default_model_type <- ifelse(
        "enrichment" %in% names(modeling_results),
        "enrichment",
        names(modeling_results)[1]
    )

    red_dim_names <- reducedDimNames(spe)
    if (length(red_dim_names) > 0) {
        red_dim_names <- sort(red_dim_names)
    } else {
        red_dim_names <- NULL
    }

    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(docs_path),
        # List the first level UI elements here
        navbarPage(
            title = title,
            tabPanel("Overview", tagList(includeMarkdown(
                file.path(resourcePaths()["www"], "README.md")
            ))),
            tabPanel("spot-level data", tagList(
                sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = "sample",
                            label = "Sample to plot",
                            choices = unique(spe$sample_id)
                        ),
                        helpText("The sample ID to visualize in most tabs except the 'grid' ones."),
                        hr(),
                        selectInput(
                            inputId = "imageid",
                            label = "Image name",
                            choices = c("edited_imaged", unique(imgData(spe)$image_id)),
                            selected = unique(imgData(spe)$image_id)[1]
                        ),
                        helpText("The name of the background image you want to visualize."),
                        hr(),
                        selectInput(
                            inputId = "cluster",
                            label = "Discrete variable to plot",
                            choices = spe_discrete_vars,
                            selected = default_cluster
                        ),
                        helpText("Typically cluster labels or any other discrete variable."),
                        hr(),
                        pickerInput(
                            inputId = "clustercolor",
                            label = "Discrete color scale",
                            choices = c(
                                colnames(colData(spe))[grep("_colors$", colnames(colData(spe)))],
                                with(
                                    subset(
                                        paletteer::palettes_d_names,
                                        length >= length(unique(colData(spe)[[default_cluster]]))
                                    ),
                                    paste0(package, "::", palette)
                                )
                            ),
                            selected = ifelse(
                                paste0(default_cluster, "_colors") %in% colnames(colData(spe)),
                                paste0(default_cluster, "_colors"),
                                "Polychrome::palette36"
                            ),
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        helpText("Either any columns that end with '_colors' or options from the 'paletteer' R package. Only valid options are shown."),
                        HTML("You can visually explore these options at the <a href='https://emilhvitfeldt.github.io/r-color-palettes/discrete.html'>Palette Picker</a> website."),
                        checkboxInput(
                            "clustercolor_direction",
                            "Should the color order be reversed?",
                            value = FALSE
                        ),
                        hr(),
                        pickerInput(
                            inputId = "geneid",
                            label = "Continuous variable to plot",
                            choices = c(
                                golem::get_golem_options("spe_continuous_vars"),
                                sort(rowData(spe)$gene_search)
                            ),
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        helpText("Typically a gene or any other continuous variable."),
                        hr(),
                        selectInput(
                            inputId = "assayname",
                            label = "Gene scale",
                            choices = assayNames(spe),
                            selected = assayNames(spe)[length(assayNames(spe))]
                        ),
                        helpText("The name of the gene values you want to visualize."),
                        hr(),
                        numericInput(
                            inputId = "minCount",
                            label = "Minimum count value",
                            value = 0,
                            min = -1,
                            max = max(assays(spe)[[length(assayNames(spe))]]),
                            step = 1
                        ),
                        helpText(
                            "You can manually enter any number then press enter in your keyboard. This is useful for extreme values."
                        ),
                        hr(),
                        selectInput(
                            inputId = "genecolor",
                            label = "Gene color scale",
                            choices = c("viridis", "paper", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "turbo"),
                            selected = "viridis"
                        ),
                        helpText("The viridis scale is color-blind friendly."),
                        checkboxInput(
                            "genecolor_direction",
                            "Should the colors be ordered from darkest to lightest?",
                            value = TRUE
                        ),
                        hr(),
                        numericInput(
                            "alphalevel",
                            "Spot transparency level",
                            value = 1,
                            min = 0,
                            max = 1,
                            step = 0.1
                        ),
                        helpText("Use values between 0 (transparent) to 1 (full color)."),
                        hr(),
                        numericInput(
                            "pointsize",
                            "Spot point size",
                            value = 1.25,
                            min = 1,
                            max = 3,
                            step = 0.1
                        ),
                        helpText("We recommend that you use values between 1.25 (default) and 2."),
                        hr(),
                        selectInput(
                            inputId = "reduced_name",
                            label = "Reduced dimensions",
                            choices = red_dim_names,
                            selected = red_dim_names[length(red_dim_names)]
                        ),
                        helpText("The first two dimensions are shown in the 'clusters (interactive)' tab."),
                        hr(),
                        checkboxInput("dropNA",
                            "Drop NA layer entries in the CSV file?",
                            value = TRUE
                        ),
                        downloadButton("downloadData", "Download manual annotations"),
                        helpText(
                            "Save your manual annotations frequently to avoid losing your work!"
                        ),
                        hr(),
                        fileInput(
                            "priorGuesses",
                            'Overwrite "ManualAnnotation" with your prior annotations. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                            accept = c(
                                "text/csv",
                                ".csv",
                                "text/comma-separated-values,text/plain"
                            )
                        ),
                        helpText(
                            "This is useful for resuming your work. It should be a CSV file with the sample_id, spot_name, and ManualAnnotation columns."
                        ),
                        hr(),
                        width = 2
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                "Documentation",
                                tags$br(),
                                HTML(
                                    'Basic information overview about the spot-level SingleCellExperiment object. You can download it using <code>spatialLIBD::fetch_data(type = "spe")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput("raw_summary"),
                                helpText(
                                    "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
                                ),
                                tags$br(),
                                hr(),
                                includeMarkdown(file.path(
                                    resourcePaths()["www"], "documentation_spe.md"
                                )),
                                hr()
                            ),
                            tabPanel(
                                "Clusters (static)",
                                checkboxInput(
                                    "side_by_side_histology",
                                    "Show clusters and background image side by side.",
                                    value = FALSE,
                                    width = "100%"
                                ),
                                downloadButton("downloadPlotHistology", "Download PDF"),
                                plotOutput("histology"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Clusters (interactive)",
                                plotlyOutput("histology_plotly",
                                    width = "1200px",
                                    height = "1200px"
                                ),
                                helpText("This is a 2 by 2 plotting area with the top row showing the spatially-resolved data on top of the histology images. The bottom row shows the data on the reduced dimensions. The left column shows the selected continuous variable (typically a gene) while the right column shows the selected discreate variable (typically cluster annotations)."),
                                textInput("label_manual_ann", "Manual annotation label", "Your Guess"),
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
                                helpText("Select points (lasso) to label them with a manual annotation."),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Clusters grid (static)",
                                checkboxGroupInput(
                                    "grid_samples",
                                    label = "Select samples to show in the grid",
                                    choices = unique(spe$sample_id),
                                    selected = unique(spe$sample_id)[seq(1, 11, by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    "grid_nrow",
                                    label = "N rows",
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    "grid_ncol",
                                    label = "N columns",
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton("grid_update", label = "Update grid plot"),
                                downloadButton("downloadPlotHistologyGrid", "Download PDF"),
                                uiOutput("grid_static"),
                                helpText("Click the 'upgrade grid plot' button above to re-make this plot."),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene (static)",
                                checkboxInput(
                                    "side_by_side_gene",
                                    "Show gene and background image side by side.",
                                    value = FALSE,
                                    width = "100%"
                                ),
                                downloadButton("downloadPlotGene", "Download PDF"),
                                plotOutput("gene"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene (interactive)",
                                uiOutput("gene_plotly_cluster_subset_ui"),
                                plotlyOutput("gene_plotly",
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
                                helpText("Select points to label them with a manual annotation."),
                                helpText(
                                    "Note that only spots passing the minimum count value will be updated."
                                ),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Gene grid (static)",
                                checkboxGroupInput(
                                    "gene_grid_samples",
                                    label = "Select samples to show in the grid",
                                    choices = unique(spe$sample_id),
                                    selected = unique(spe$sample_id)[seq(1, length(unique(spe$sample_id)), by = 4)],
                                    inline = TRUE
                                ),
                                numericInput(
                                    "gene_grid_nrow",
                                    label = "N rows",
                                    value = 1,
                                    min = 1,
                                    max = 3
                                ),
                                numericInput(
                                    "gene_grid_ncol",
                                    label = "N columns",
                                    value = 3,
                                    min = 1,
                                    max = 4
                                ),
                                actionButton("gene_grid_update", label = "Update grid plot"),
                                downloadButton("downloadPlotGeneGrid", "Download PDF"),
                                uiOutput("gene_grid_static"),
                                helpText("Click the 'upgrade grid plot' button above to re-make this plot."),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Edit image",
                                helpText("Edit the selected image by manipulating the colors and apperance, which can be useful when inspecting the selected image from the left menu ('image name'). Once you have a set of edits you like, click the 'update custom image' button below to save your edits. Next, select on the left menu ('image name') the 'edited_image' option to use your new image as the background image in the rest of the visualizations. Most of these image manipulations are explained at", HTML("<a href='https://docs.ropensci.org/magick/reference/color.html'>the magick R package documentation</a>.")),
                                helpText("If you want a uniform colored background, set the brightness to 0 which will make it black, then either proceeed or select the 'negate' checkbox for white, click the 'edit custom image' button, and select the input 'image name' as 'edited image'. Instead of using 'negate' you could use 'transparent (color)' and type 'black' then under 'background (color)' type a valid R color name such as 'purple' or 'lightblue' or a color HEX value such as '#e1eb34'."),
                                hr(),
                                fluidRow(
                                    column(
                                        width = 4,
                                        selectInput(
                                            inputId = "editImg_channel",
                                            label = "Select an image channel such as 'Red' or 'Blue'",
                                            choices = c("", magick::channel_types()),
                                            selected = ""
                                        ),
                                        helpText("Leave this empty if you don't want to select a channel. Note that the definition of channel here is different from a multi-channel image from say VisiumIF."),
                                        hr(),
                                        numericInput(
                                            "editImg_brightness",
                                            label = "Image brightness level",
                                            value = 100,
                                            min = 0,
                                            max = 100
                                        ),
                                        numericInput(
                                            "editImg_saturation",
                                            label = "Image saturation level",
                                            value = 100,
                                            min = 0,
                                            max = 100
                                        ),
                                        numericInput(
                                            "editImg_hue",
                                            label = "Image hue level",
                                            value = 100,
                                            min = 0,
                                            max = 200
                                        ),
                                        helpText("Modulate the colors in the image. Brightness and saturation are in percents while hue has a range of 0 to 200. Use 100 for all 3 options for no change.")
                                    ),
                                    column(
                                        width = 4,
                                        checkboxInput(
                                            "editImg_enhance",
                                            "enhance: attempt to minimize noise",
                                            value = FALSE
                                        ),
                                        checkboxInput(
                                            "editImg_normalize",
                                            "normalize: increases contrast by normalizing the pixel values to span the full range of colors",
                                            value = FALSE
                                        ),
                                        hr(),
                                        numericInput(
                                            "editImg_contrast_sharpen",
                                            label = "contrast (sharpen): enhance intensity differences in image",
                                            value = NA,
                                            min = -100,
                                            max = 100
                                        ),
                                        helpText("Try with 1 to start with."),
                                        hr(),
                                        numericInput(
                                            "editImg_quantize_max",
                                            label = "quantize (max): reduce number of colors in the image",
                                            value = NA,
                                            min = 1
                                        ),
                                        checkboxInput(
                                            "editImg_quantize_dither",
                                            "quantize (dither): whether to apply Floyd/Steinberg error diffusion to the image: averages intensities of several neighboring pixels",
                                            value = TRUE
                                        ),
                                        helpText("You could try 256 colors or a much small number like 25 or 40.")
                                    ),
                                    column(
                                        width = 4,
                                        checkboxInput(
                                            "editImg_equalize",
                                            "equalize: whether to use histogram equalization",
                                            value = FALSE
                                        ),
                                        hr(),
                                        textInput(
                                            "editImg_transparent_color",
                                            label = "transparent (color): set pixels approximately matching given color",
                                            value = NA
                                        ),
                                        numericInput(
                                            "editImg_transparent_fuzz",
                                            label = "transparent (fuzz): relative color distance (value between 0 and 100) to be considered similar",
                                            value = 0,
                                            min = 0,
                                            max = 100
                                        ),
                                        helpText("Type 'purple' and select a fuzz of 25 to start with."),
                                        textInput(
                                            "editImg_background_color",
                                            label = "background (color): sets background color",
                                            value = NA
                                        ),
                                        hr(),
                                        numericInput(
                                            "editImg_median_radius",
                                            label = "median (radius): replace each pixel with the median color in a circular neighborhood",
                                            value = NA,
                                            min = 0
                                        ),
                                        helpText("Choose a small radius, like 1 or 2 to start. The higher the value, the longer this computation will take."),
                                        hr(),
                                        checkboxInput(
                                            "editImg_negate",
                                            "negate: whether to negate colors",
                                            value = FALSE
                                        )
                                    )
                                ),
                                actionButton("editImg_reset_menus", label = "Reset menus"),
                                helpText("Reset all image editing menus to their default values."),
                                hr(),
                                downloadButton("downloadPlotEditImg", "Download PDF"),
                                plotOutput("editImg_plot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                actionButton("editImg_update", label = "Update custom image"),
                                helpText("Click the 'upgrade custom image' button above to save the custom image. You can then select 'edited_image' and use it in other parts of the web application. Note that if you had 'edited_image' already selected, you'll need to re-select or change another input to update the other plots."),
                                checkboxInput(
                                    "editImg_overwrite",
                                    "Whether to overwrite the 'edited_image'",
                                    value = FALSE
                                ),
                                helpText("Select if you want to do sequential image manipulations when you have selected as input the 'edited_image'.")
                            )
                        )
                    )
                )
            )),
            if (!is.null(sce_layer)) {
                tabPanel("layer-level data", tagList(sidebarLayout(
                    sidebarPanel(
                        selectInput(
                            inputId = "layer_model",
                            label = "Model results",
                            choices = names(modeling_results),
                            selected = default_model_type
                        ),
                        hr(),
                        pickerInput(
                            inputId = "layer_geneid",
                            label = "Gene",
                            choices =
                                sort(rowData(sce_layer)$gene_search),
                            options = pickerOptions(liveSearch = TRUE)
                        ),
                        hr(),
                        width = 2
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                "Documentation",
                                tags$br(),
                                HTML(
                                    'Basic information overview about the layer-level SingleCellExperiment object (pseudo-bulked from the spot-level data). You can download it using <code>spatialLIBD::fetch_data(type = "sce_layer")</code>.'
                                ),
                                tags$br(),
                                tags$br(),
                                verbatimTextOutput("layer_raw_summary"),
                                helpText(
                                    "When this information has been displayed it means that the shiny web application has finished loading and you can start exploring the rest of it."
                                ),
                                tags$br(),
                                hr(),
                                includeMarkdown(
                                    file.path(resourcePaths()["www"], "documentation_sce_layer.md")
                                ),
                            ),
                            tabPanel(
                                "Reduced Dim",
                                selectInput(
                                    inputId = "layer_which_dim",
                                    label = "Reduced Dimension",
                                    choices = sort(reducedDimNames(sce_layer)),
                                    selected = ifelse(
                                        "PCA" %in% reducedDimNames(sce_layer),
                                        "PCA",
                                        reducedDimNames(sce_layer)[1]
                                    )
                                ),
                                selectInput(
                                    inputId = "layer_which_dim_color",
                                    label = "Color by",
                                    choices = sort(
                                        colnames(colData(sce_layer))[
                                            !grepl("_colors$", colnames(colData(sce_layer)))
                                        ]
                                    ),
                                    selected = default_cluster
                                ),
                                downloadButton("layer_downloadReducedDim", "Download PDF"),
                                plotOutput("layer_reduced_dim"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br()
                            ),
                            tabPanel(
                                "Model boxplots",
                                fluidRow(
                                    column(
                                        width = 6,
                                        selectInput(
                                            inputId = "layer_model_test",
                                            label = "Model test",
                                            choices = sort(unique(sig_genes$test[sig_genes$model_type == default_model_type]))
                                        ),
                                        helpText("Short label for the statistical test done. For the model type 'enrichment', 'model test' is the selected group against all other ones. For 'pairwise' it's 'G1-G2' where the t-statistics are G1 > G2. For 'anova', it's an F-statistic."),
                                        selectInput(
                                            inputId = "layer_boxcolor",
                                            label = "Boxplot color scale",
                                            choices = c("viridis", "paper", "bluered"),
                                            selected = "viridis"
                                        ),
                                        helpText("'viridis' are color-blind friendly colors (default). 'paper' are the ones used in Maynard et al, Nature Neurosci, 2021. 'bluered' are blue and red colors.")
                                    ),
                                    column(
                                        width = 6,
                                        selectInput(
                                            inputId = "layer_model_assayname",
                                            label = "Assay name",
                                            choices = assayNames(sce_layer),
                                            selected = ifelse(
                                                "logcounts" %in% assayNames(sce_layer),
                                                "logcounts",
                                                assayNames(sce_layer)[1]
                                            )
                                        ),
                                        helpText("Type of gene expression values you would like to see. Typical options are 'counts' for raw counts and 'logcounts' for log normalized counts."),
                                        numericInput(
                                            inputId = "layer_box_cex",
                                            label = "Font size",
                                            value = 2.7,
                                            min = 1,
                                            max = 5,
                                            step = 0.1
                                        ),
                                        helpText("Select a smaller value if your group labels are too long. Default: 2.7."),
                                        checkboxInput(
                                            "layer_box_shortitle",
                                            "Enable short title on boxplots.",
                                            value = TRUE
                                        ),
                                        helpText("Delect if you want a longer title that includes the gene ID (typically ENSEMBL) and the t or F statistic value.")
                                    )
                                ),
                                hr(),
                                downloadButton("layer_downloadBoxplot", "Download PDF"),
                                plotOutput("layer_boxplot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                tags$h2("Selected model and gene across all tests"),
                                downloadButton("layer_downloadModelTable", "Download CSV"),
                                helpText("This table shows the selected gene across all 'test's for the selected 'model type'. This information is useful if you want to quickly check the results for other 'test's under the same 'model type' context."),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_model_table"),
                                 hr(),
                                tags$h2("Selected model and test across all genes"),
                                downloadButton("layer_downloadModelTable_full", "Download CSV"),
                                helpText("This table shows the selected 'model type' and 'test' across all genes. This table is useful if you want to find how other genes rank under the same model type and test context. It can be useful to find genes to select."),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_model_table_full"),
                            ),
                            tabPanel(
                                "Gene Set Enrichment",
                                fileInput(
                                    "geneSet",
                                    "Upload a CSV file with one column per gene set with a header row and then Ensembl gene IDs as values.",
                                    accept = c(
                                        "text/csv",
                                        ".csv",
                                        "text/comma-separated-values,text/plain"
                                    )
                                ),
                                helpText(
                                    "It should be a CSV file without row names and similar to ",
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/asd_sfari_geneList.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    "layer_gene_fdrcut",
                                    label = "FDR cutoff",
                                    value = 0.1,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                hr(),
                                downloadButton("layer_downloadGeneSet", "Download PDF"),
                                plotOutput("layer_gene_set_plot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                downloadButton("layer_downloadGeneSetTable", "Download CSV"),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_gene_set_table")
                            ),
                            tabPanel(
                                "Spatial registration",
                                fileInput(
                                    "externalTstat",
                                    "Upload a CSV file with one column per cell type or layer that contains the enrichment t-stat equivalent and with Ensembl gene IDs as the row names.",
                                    accept = c(
                                        "text/csv",
                                        ".csv",
                                        "text/comma-separated-values,text/plain"
                                    )
                                ),
                                helpText(
                                    "It should be a CSV file similar to ",
                                    HTML(
                                        '<a href="https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv">this example file.</a>'
                                    )
                                ),
                                hr(),
                                numericInput(
                                    "layer_tstat_max",
                                    label = "Maximum correlation",
                                    value = 0.81,
                                    min = 0,
                                    max = 1,
                                    step = 0.01
                                ),
                                helpText("Use a smaller positive number to change the range of the color scale used."),
                                hr(),
                                downloadButton("layer_downloadTstatCor", "Download PDF"),
                                plotOutput("layer_tstat_cor_plot"),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                tags$br(),
                                hr(),
                                downloadButton("layer_downloadTstatCorTable", "Download CSV"),
                                helpText("Correlation matrix that is visually illustrated with the previous plot."),
                                tags$br(),
                                tags$br(),
                                DT::DTOutput("layer_tstat_cor_table")
                            )
                        )
                    )
                )))
            } else {
                NULL
            },
            tabPanel(
                "Help or feedback",
                tagList(
                    HTML(
                        'Please get in touch with the <code>spatialLIBD</code> authors through the <a href="https://support.bioconductor.org/">Bioconductor Support Website</a> (using the <code>spatialLIBD</code> <a href="https://support.bioconductor.org/t/spatialLIBD/">tag</a>) or through <a href="https://github.com/LieberInstitute/spatialLIBD/issues">GitHub</a>. Remember to help others help you by including all the information required to reproduce the problem you noticed. Thank you!'
                    ),
                    hr(),
                    p("The following information will be useful to them:"),
                    verbatimTextOutput("session_info")
                )
            ),
            footer = tagList(
                hr(),
                includeHTML(
                    file.path(resourcePaths()["www"], "footer.html")
                ),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br()
            )
        )
    )
}

#' @import shiny
golem_add_external_resources <- function(docs_path) {
    addResourcePath(
        "www",
        docs_path
    )

    tags$head(
        golem::activate_js(),
        # Add here all the external resources
        # If you have a custom.css in the inst/app/www
        # Or for example, you can add shinyalert::useShinyalert() here
        # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
        golem::favicon()
    )
}
