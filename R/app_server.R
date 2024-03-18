#' @import shiny
#' @import SingleCellExperiment
#' @import ggplot2
#' @import grid
#' @import paletteer
#' @importFrom cowplot plot_grid
#' @importFrom viridisLite viridis
#' @importFrom sessioninfo session_info
#' @rawNamespace import(plotly, except = last_plot)
#' @importFrom grDevices as.raster pdf dev.off
#' @importFrom png readPNG
#' @importFrom scater plotReducedDim
#' @importFrom DT renderDT
#' @importFrom utils read.csv write.csv
#' @importFrom SummarizedExperiment assays

app_server <- function(input, output, session) {
    ## Some variables
    COUNT <-
        model_type <- ensembl <- key <- ManualAnnotation <- test <- NULL

    ## Get options
    spe <- golem::get_golem_options("spe")
    sce_layer <- golem::get_golem_options("sce_layer")
    modeling_results <- golem::get_golem_options("modeling_results")
    sig_genes <- golem::get_golem_options("sig_genes")
    default_cluster <- golem::get_golem_options("default_cluster")


    # List the first level callModules here

    ## Global variables needed throughout the app
    rv <- reactiveValues(ManualAnnotation = rep("NA", ncol(spe)))

    ## From /dcs04/lieber/lcolladotor/with10x_LIBD001/HumanPilot/Analysis/rda_scran/clust_10x_layer_maynard_martinowich.Rdata
    # cat(paste0("'", names(cols_layers_martinowich), "' = '", cols_layers_martinowich, "',\n"))
    cols_layers_martinowich <- c(
        "WM" = "#b2df8a",
        "6" = "#e41a1c",
        "1" = "#377eb8",
        "5" = "#4daf4a",
        "4" = "#ff7f00",
        "2_3" = "gold",
        "5_6" = "#a65628",
        "4_5" = "#999999",
        "1_6" = "black",
        "3" = "grey",
        "2" = "white",
        "1_5" = "purple"
    )

    ## For layer_guess and related variables
    cols_layers_paper <- function() {
        spatialLIBD::libd_layer_colors[-length(spatialLIBD::libd_layer_colors)]
    }
    cols_layers_paper_short <- function() {
        res <- cols_layers_paper()
        names(res) <- gsub("ayer", "", names(res))
        return(res)
    }

    observeEvent(input$cluster, {
        if (isolate(input$cluster) == "ManualAnnotation") {
            current_n <- length(unique(rv$ManualAnnotation))
        } else {
            current_n <- length(unique(colData(spe)[[isolate(input$cluster)]]))
        }

        preferred_choice <- colnames(colData(spe))[grep(paste0(isolate(input$cluster), "_colors$"), colnames(colData(spe)))]

        choices <- c(
            preferred_choice,
            with(
                subset(
                    paletteer::palettes_d_names,
                    length >= current_n
                ),
                paste0(package, "::", palette)
            )
        )

        if (length(preferred_choice) == 0) {
            preferred_choice <- ifelse(
                "Polychrome::palette36" %in% choices,
                "Polychrome::palette36",
                choices[1]
            )
        }

        updatePickerInput(
            session = session,
            inputId = "clustercolor",
            choices = choices,
            selected = preferred_choice
        )
    })

    cluster_colors <- reactive({
        if (input$cluster %in% c("Maynard", "Martinowich")) {
            colors <- cols_layers_martinowich
        } else if (input$cluster == "ManualAnnotation") {
            colors <- paletteer::paletteer_d(
                palette = input$clustercolor,
                n = length(unique(rv$ManualAnnotation)),
                direction = ifelse(input$clustercolor_direction, -1, 1)
            )
            names(colors) <- unique(rv$ManualAnnotation)
        } else if (input$cluster %in% c("layer_guess", "layer_guess_reordered")) {
            colors <- cols_layers_paper()
        } else if (input$cluster %in% c("layer_guess_reordered_short", "spatialLIBD")) {
            colors <- cols_layers_paper_short()
        } else if (input$clustercolor %in% colnames(colData(spe)) &&
            is.factor(colData(spe)[[input$cluster]])) {
            colors <-
                colData(spe)[[input$clustercolor]][unique(names(colData(spe)[[input$clustercolor]]))]
            colors <- colors[levels(colData(spe)[[input$cluster]])]
        } else if (input$clustercolor %in% colnames(colData(spe))) {
            colors <-
                colData(spe)[[input$clustercolor]][unique(names(colData(spe)[[input$clustercolor]]))]
        } else {
            colors <- paletteer::paletteer_d(
                palette = input$clustercolor,
                n = length(unique(colData(spe)[[input$cluster]])),
                direction = ifelse(input$clustercolor_direction, -1, 1)
            )
            names(colors) <- unique(colData(spe)[[input$cluster]])
        }
        return(colors)
    })

    cont_colors <- reactive({
        paper_cols <- c("aquamarine4", "springgreen", "goldenrod", "red")
        cont_colors <- if (input$genecolor == "paper") {
            if (input$genecolor_direction) {
                paper_cols
            } else {
                rev(paper_cols)
            }
        } else {
            viridis(
                21,
                option = input$genecolor,
                direction = ifelse(input$genecolor_direction, 1, -1)
            )
        }
    })

    # Set the max based on the assay
    observeEvent(input$assayname, {
        updateNumericInput(
            session,
            inputId = "minCount",
            value = 0,
            min = -1,
            max = max(assays(spe)[[input$assayname]]),
            step = 1
        )
    })

    ## Static plotting functions
    static_histology <- reactive({
        if (input$cluster == "ManualAnnotation") {
            spe$ManualAnnotation <- rv$ManualAnnotation
        }
        p <- vis_clus(
            spe,
            sampleid = input$sample,
            clustervar = input$cluster,
            colors = cluster_colors(),
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize,
            auto_crop = input$auto_crop,
            ... = paste(" with", input$cluster)
        )
        if (!input$side_by_side_histology) {
            return(p)
        } else {
            p_no_spots <- p
            p_no_spots$layers[[2]] <- NULL

            p_no_spatial <- p
            p_no_spatial$layers[[1]] <- NULL
            cowplot::plot_grid(
                plotlist = list(
                    p_no_spots,
                    p_no_spatial + ggplot2::theme(legend.position = "none")
                ),
                nrow = 1,
                ncol = 2
            )
        }
    })

    static_histology_grid <- reactive({
        input$grid_update

        if (isolate(input$cluster == "ManualAnnotation")) {
            spe$ManualAnnotation <- rv$ManualAnnotation
        }
        plots <-
            vis_grid_clus(
                spe,
                isolate(input$cluster),
                sort_clust = FALSE,
                colors = isolate(cluster_colors()),
                return_plots = TRUE,
                spatial = isolate(input$grid_spatial_clus),
                image_id = isolate(input$imageid),
                alpha = isolate(input$alphalevel),
                sample_order = isolate(input$grid_samples),
                point_size = isolate(input$pointsize),
                auto_crop = isolate(input$auto_crop),
                ... = paste(" with", isolate(input$cluster))
            )
        cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$grid_nrow),
            ncol = isolate(input$grid_ncol)
        )
    })

    static_gene <- reactive({
        p <- vis_gene(
            spe,
            sampleid = input$sample,
            geneid = input$geneid,
            multi_gene_method = input$multi_gene_method,
            assayname = input$assayname,
            minCount = input$minCount,
            cont_colors = cont_colors(),
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize,
            auto_crop = input$auto_crop
        )
        if (!input$side_by_side_gene) {
            return(p)
        } else {
            p_no_spots <- p
            p_no_spots$layers[[2]] <- NULL

            p_no_spatial <- p
            p_no_spatial$layers[[1]] <- NULL
            cowplot::plot_grid(
                plotlist = list(
                    p_no_spots,
                    p_no_spatial + ggplot2::theme(legend.position = "none")
                ),
                nrow = 1,
                ncol = 2
            )
        }
    })

    static_gene_grid <- reactive({
        input$gene_grid_update

        plots <-
            vis_grid_gene(
                spe,
                geneid = isolate(input$geneid),
                multi_gene_method = input$multi_gene_method,
                assayname = isolate(input$assayname),
                minCount = isolate(input$minCount),
                return_plots = TRUE,
                spatial = isolate(input$grid_spatial_gene),
                cont_colors = isolate(cont_colors()),
                image_id = isolate(input$imageid),
                alpha = isolate(input$alphalevel),
                point_size = isolate(input$pointsize),
                sample_order = isolate(input$gene_grid_samples),
                auto_crop = isolate(input$auto_crop)
            )
        cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$gene_grid_nrow),
            ncol = isolate(input$gene_grid_ncol)
        )
    })

    editImg_manipulations <- reactive({
        edited_imaged <-
            img_edit(
                spe,
                sampleid = input$sampleid,
                image_id = input$imageid,
                channel = input$editImg_channel,
                brightness = input$editImg_brightness,
                saturation = input$editImg_saturation,
                hue = input$editImg_hue,
                enhance = input$editImg_enhance,
                normalize = input$editImg_normalize,
                contrast_sharpen = input$editImg_contrast_sharpen,
                quantize_max = input$editImg_quantize_max,
                quantize_dither = input$editImg_quantize_dither,
                equalize = input$editImg_equalize,
                transparent_color = input$editImg_transparent_color,
                transparent_fuzz = input$editImg_transparent_fuzz,
                background_color = input$editImg_background_color,
                median_radius = input$editImg_median_radius,
                negate = input$editImg_negate
            )
        as.raster(edited_imaged)
    })

    observeEvent(input$editImg_update, {
        spe <<-
            img_update_all(
                spe,
                image_id = input$imageid,
                new_image_id = "edited_imaged",
                overwrite = input$editImg_overwrite,
                channel = input$editImg_channel,
                brightness = input$editImg_brightness,
                saturation = input$editImg_saturation,
                hue = input$editImg_hue,
                enhance = input$editImg_enhance,
                normalize = input$editImg_normalize,
                contrast_sharpen = input$editImg_contrast_sharpen,
                quantize_max = input$editImg_quantize_max,
                quantize_dither = input$editImg_quantize_dither,
                equalize = input$editImg_equalize,
                transparent_color = input$editImg_transparent_color,
                transparent_fuzz = input$editImg_transparent_fuzz,
                background_color = input$editImg_background_color,
                median_radius = input$editImg_median_radius,
                negate = input$editImg_negate
            )
    })

    observeEvent(input$editImg_reset_menus, {
        ## Column 1
        updateSelectInput(inputId = "editImg_channel", selected = "")
        updateNumericInput(inputId = "editImg_brightness", value = 100)
        updateNumericInput(inputId = "editImg_saturation", value = 100)
        updateNumericInput(inputId = "editImg_hue", value = 100)

        ## Column 2
        updateCheckboxInput(inputId = "editImg_enhance", value = FALSE)
        updateCheckboxInput(inputId = "editImg_normalize", value = FALSE)
        updateNumericInput(inputId = "editImg_contrast_sharpen", value = NA)
        updateNumericInput(inputId = "editImg_quantize_max", value = NA)
        updateCheckboxInput(inputId = "editImg_quantize_dither", value = TRUE)

        ## Column 3
        updateCheckboxInput(inputId = "editImg_equalize", value = FALSE)
        updateTextInput(inputId = "editImg_transparent_color", value = NA)
        updateNumericInput(inputId = "editImg_transparent_fuzz", value = 0)
        updateTextInput(inputId = "editImg_background_color", value = NA)
        updateNumericInput(inputId = "editImg_median_radius", value = NA)
        updateCheckboxInput(inputId = "editImg_negate", value = FALSE)
    })



    ## Download static plots as PDFs
    output$downloadPlotHistology <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_static_cluster_",
                    input$cluster,
                    "_",
                    input$sample,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 9 * ifelse(input$side_by_side_histology, 2, 1)
            )
            print(static_histology())
            dev.off()
        }
    )

    output$downloadPlotHistologyGrid <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_static_cluster_grid_",
                    input$cluster,
                    "_",
                    paste0(input$grid_samples, collapse = "_"),
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8 * isolate(input$grid_nrow),
                width = 9 * isolate(input$grid_ncol)
            )
            print(static_histology_grid())
            dev.off()
        }
    )

    output$downloadPlotGene <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_static_gene_",
                    paste0(input$geneid, collapse = "_"),
                    "_",
                    input$sample,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 8 * ifelse(input$side_by_side_gene, 2, 1)
            )
            print(static_gene())
            dev.off()
        }
    )

    output$downloadPlotGeneGrid <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_static_gene_grid_",
                    paste0(input$geneid, collapse = "_"),
                    "_",
                    paste0(input$grid_samples, collapse = "_"),
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8 * isolate(input$gene_grid_nrow),
                width = 8 * isolate(input$gene_grid_ncol)
            )
            print(static_gene_grid())
            dev.off()
        }
    )

    output$downloadPlotEditImg <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_editedImage_",
                    input$imageid,
                    "_",
                    input$sample,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 8
            )
            plot(editImg_manipulations())
            dev.off()
        }
    )

    ## Clusters/Layers
    output$histology <- renderPlot(
        {
            static_histology()
        },
        width = function() {
            600 * ifelse(input$side_by_side_histology, 2, 1)
        },
        height = 600
    )


    output$grid_static <- renderUI({
        input$grid_update

        plotOutput(
            "histology_grid",
            width = 600 * isolate(input$grid_ncol),
            height = 600 * isolate(input$grid_nrow)
        )
    })

    output$histology_grid <- renderPlot(
        {
            print(static_histology_grid())
        },
        width = "auto",
        height = "auto"
    )


    output$gene <- renderPlot(
        {
            static_gene()
        },
        width = function() {
            600 * ifelse(input$side_by_side_gene, 2, 1)
        },
        height = 600
    )


    output$gene_grid_static <- renderUI({
        input$gene_grid_update

        plotOutput(
            "gene_grid",
            width = 600 * isolate(input$gene_grid_ncol),
            height = 600 * isolate(input$gene_grid_nrow)
        )
    })

    output$gene_grid <- renderPlot(
        {
            print(static_gene_grid())
        },
        width = "auto",
        height = "auto"
    )

    output$editImg_plot <- renderPlot(
        {
            plot(editImg_manipulations())
        },
        width = 600,
        height = 600
    )


    ## Plotly versions
    output$histology_plotly <- renderPlotly({
        if (input$cluster == "ManualAnnotation") {
            spe$ManualAnnotation <- rv$ManualAnnotation
        }
        colors <- cluster_colors()

        ## Define some common arguments
        sampleid <- input$sample
        geneid <- input$geneid
        assayname <- input$assayname
        minCount <- input$minCount
        clustervar <- input$cluster
        reduced_name <- input$reduced_name
        genecolor <- input$genecolor
        #
        # ## Testing:
        # sampleid <- '151507'
        # geneid <- "SCGB2A2; ENSG00000110484"
        # assayname <- 'logcounts'
        # minCount <- 0
        # clustervar <- 'GraphBased'
        # colors <- get_colors(NULL, spe$GraphBased)
        # reduced_name <- 'TSNE_perplexity50'
        # genecolor <- "viridis"

        ## Read in the histology image
        img <-
            SpatialExperiment::imgRaster(spe,
                sample_id = sampleid,
                image_id = input$imageid
            )
        if (input$auto_crop) {
            frame_lims <-
                frame_limits(spe,
                    sampleid = sampleid,
                    image_id = input$imageid
                )
            img <-
                img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
        }

        ## From vis_gene() in global.R
        spe_sub <- spe[, spe$sample_id == sampleid]
        d <-
            as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$sample_id == sampleid, ],
                optional = TRUE
            )
        #   Grab any continuous colData columns
        cont_cols <- as.matrix(
            colData(spe_sub)[
                , geneid[geneid %in% colnames(colData(spe_sub))],
                drop = FALSE
            ]
        )

        #   Get the integer indices of each gene in the SpatialExperiment, since we
        #   aren't guaranteed that rownames are gene names
        remaining_geneid <- geneid[!(geneid %in% colnames(colData(spe_sub)))]
        valid_gene_indices <- unique(
            c(
                match(remaining_geneid, rowData(spe_sub)$gene_search),
                match(remaining_geneid, rownames(spe_sub))
            )
        )
        valid_gene_indices <- valid_gene_indices[!is.na(valid_gene_indices)]

        #   Grab any genes
        gene_cols <- t(
            as.matrix(assays(spe_sub[valid_gene_indices, ])[[assayname]])
        )

        #   Combine into one matrix where rows are genes and columns are continuous
        #   features
        cont_matrix <- cbind(cont_cols, gene_cols)

        #   Determine plot and legend titles
        if (ncol(cont_matrix) == 1) {
            if (!(geneid %in% colnames(colData(spe_sub)))) {
                plot_title <- sprintf(
                    "%s %s %s min > %s", sampleid, geneid, assayname, minCount
                )
            } else {
                plot_title <- sprintf(
                    "%s %s min > %s", sampleid, geneid, minCount
                )
            }
            d$COUNT <- cont_matrix[, 1]
        } else {
            if (input$multi_gene_method == "z_score") {
                d$COUNT <- multi_gene_z_score(cont_matrix)
                plot_title <- paste(sampleid, "Z-score min > ", minCount)
            } else if (input$multi_gene_method == "sparsity") {
                d$COUNT <- multi_gene_sparsity(cont_matrix)
                plot_title <- paste(sampleid, "Prop. nonzero min > ", minCount)
            } else { # must be 'pca'
                d$COUNT <- multi_gene_pca(cont_matrix)
                plot_title <- paste(sampleid, "PC1 min >", minCount)
            }
        }
        d$COUNT[d$COUNT <= minCount] <- NA

        ## Add the reduced dims
        if (reduced_name != "") {
            red_dims <-
                reducedDim(spe, reduced_name)[spe$sample_id == sampleid, ]
            colnames(red_dims) <-
                paste(reduced_name, "dim", seq_len(ncol(red_dims)))
            d <- cbind(d, red_dims)
        }

        ## Drop points below minCount
        d <- subset(d, !is.na(COUNT))

        ## Use client-side highlighting
        d_key <- highlight_key(d, ~key)

        ## Make the cluster plot
        p_clus <- vis_clus_p(
            spe = spe,
            d = d_key,
            clustervar = clustervar,
            sampleid = sampleid,
            colors = get_colors(colors, d[, clustervar]),
            spatial = FALSE,
            title = plot_title,
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize,
            auto_crop = input$auto_crop
        )

        ## Next the gene plot
        p_gene <- vis_gene_p(
            spe = spe,
            d = d_key,
            sampleid = sampleid,
            spatial = FALSE,
            title = "",
            cont_colors = cont_colors(),
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize,
            auto_crop = input$auto_crop
        ) + geom_point(
            shape = 21,
            size = input$pointsize,
            stroke = 0,
            alpha = input$alphalevel
        )

        ## Make the reduced dimensions ggplot
        if (reduced_name != "") {
            p_dim <- ggplot(
                d_key,
                aes(
                    x = !!sym(colnames(red_dims)[1]),
                    y = !!sym(colnames(red_dims)[2]),
                    fill = factor(!!sym(clustervar)),
                    key = key
                )
            ) +
                geom_point(
                    shape = 21,
                    size = input$pointsize,
                    stroke = 0
                ) +
                scale_fill_manual(values = get_colors(colors, colData(spe)[[clustervar]][spe$sample_id == sampleid])) +
                guides(fill = "none") +
                ggtitle("") +
                theme_set(theme_bw(base_size = 20)) +
                theme(
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank()
                )

            p_dim_gene <- ggplot(
                d_key,
                aes(
                    x = !!sym(colnames(red_dims)[1]),
                    y = !!sym(colnames(red_dims)[2]),
                    fill = COUNT,
                    color = COUNT,
                    key = key
                )
            ) +
                geom_point(
                    shape = 21,
                    size = input$pointsize,
                    stroke = 0
                )
        } else {
            p_dim <- p_dim_gene <- ggplot(d_key, aes(key = key))
        }

        p_dim_gene <- p_dim_gene + scale_fill_gradientn(
            colors = cont_colors(),
            na.value = "#CCCCCC40",
            guide = "none"
        ) + scale_color_gradientn(
            colors = cont_colors(),
            na.value = "#CCCCCC40",
            guide = "none"
        )


        p_dim_gene <- p_dim_gene +
            ggtitle("") +
            theme_set(theme_bw(base_size = 20)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )

        ## Make the plotly objects with histology in the background
        x_scale <- 1.2
        plotly_clus <- layout(
            ggplotly(
                p_clus,
                width = 600 * 2 * x_scale,
                height = 600 * 2,
                source = "plotly_histology",
                tooltip = c("fill", "key")
            ),
            images = list(
                list(
                    source = raster2uri(img),
                    layer = "below",
                    xanchor = "left",
                    yanchor = "bottom",
                    xref = "x",
                    yref = "y",
                    sizing = "stretch",
                    x = 0,
                    y = -nrow(img),
                    sizex = ncol(img),
                    sizey = nrow(img),
                    opacity = 0.8
                )
            ),
            dragmode = "select"
        )

        plotly_gene <- layout(
            ggplotly(
                p_gene,
                source = "plotly_histology",
                tooltip = c("fill", "key")
            ),
            images = list(
                list(
                    source = raster2uri(img),
                    layer = "below",
                    xanchor = "left",
                    yanchor = "bottom",
                    xref = "x",
                    yref = "y",
                    sizing = "stretch",
                    x = 0,
                    y = -nrow(img),
                    sizex = ncol(img),
                    sizey = nrow(img),
                    opacity = 0.8
                )
            ),
            dragmode = "select"
        )

        plotly_dim <-
            layout(ggplotly(
                p_dim + theme(legend.position = "none"),
                source = "plotly_histology",
                tooltip = c("fill", "key")
            ))

        plotly_dim_gene <- layout(ggplotly(
            p_dim_gene,
            source = "plotly_histology",
            tooltip = c("fill", "key")
        ))

        ## It's 0.5, but I want this smaller so the cluster
        ## labels will fit
        ## Not needed now that I moved the legend further right using 'x'
        # plotly_gene$x$data[[2]]$marker$colorbar$len <- 0.5

        plotly_merged <- layout(
            subplot(
                subplot(
                    plotly_gene,
                    plotly_clus,
                    nrows = 1,
                    shareX = TRUE,
                    shareY = TRUE,
                    which_layout = 2
                ),
                style(
                    subplot(
                        plotly_dim_gene,
                        plotly_dim,
                        nrows = 1,
                        shareX = TRUE,
                        shareY = TRUE,
                        which_layout = 2
                    ),
                    showlegend = FALSE
                ),
                nrows = 2,
                shareX = FALSE,
                shareY = FALSE,
                which_layout = 1
            ),
            legend = list(tracegroupgap = 0, x = x_scale)
        )

        ## Restore some axis titles for the reduced dim plot
        plotly_merged$x$layout$xaxis3$title <-
            plotly_merged$x$layout$xaxis4$title <-
            plotly_dim$x$layout$xaxis$title
        plotly_merged$x$layout$yaxis2$title <-
            plotly_dim$x$layout$yaxis$title

        ## Make the linked (client-side) plot
        suppressMessages(suppressWarnings(toWebGL(
            highlight(plotly_merged,
                on = "plotly_selected",
                off = "plotly_deselect"
            )
        )))
    })

    ## Set the cluster subset options
    output$gene_plotly_cluster_subset_ui <- renderUI({
        input$cluster

        if (input$cluster == "ManualAnnotation") {
            cluster_opts <- unique(rv$ManualAnnotation)
        } else {
            cluster_opts <- unique(colData(spe)[[input$cluster]])
        }
        checkboxGroupInput(
            "gene_plotly_cluster_subset",
            label = "Select clusters to show in this plot",
            choices = sort(cluster_opts),
            selected = cluster_opts,
            inline = TRUE
        )
    })

    output$gene_plotly <- renderPlotly({
        if (is.null(input$gene_plotly_cluster_subset)) {
            return(NULL)
        }

        if (input$cluster == "ManualAnnotation") {
            cluster_opts <-
                rv$ManualAnnotation %in% input$gene_plotly_cluster_subset
        } else {
            cluster_opts <-
                as.character(colData(spe)[[input$cluster]]) %in% input$gene_plotly_cluster_subset
        }
        ## For when you change the input$cluster and no data is available yet
        if (sum(cluster_opts) == 0) {
            return(NULL)
        }

        p <-
            vis_gene(
                spe[, cluster_opts],
                sampleid = input$sample,
                geneid = input$geneid,
                multi_gene_method = input$multi_gene_method,
                assayname = input$assayname,
                minCount = input$minCount,
                spatial = FALSE,
                cont_colors = cont_colors(),
                image_id = input$imageid,
                alpha = input$alphalevel,
                point_size = input$pointsize,
                auto_crop = input$auto_crop
            ) +
            geom_point(
                shape = 21,
                size = input$pointsize,
                stroke = 0,
                alpha = input$alphalevel
            )

        ## Read in the histology image
        img <-
            SpatialExperiment::imgRaster(spe,
                sample_id = input$sample,
                image_id = input$imageid
            )
        if (input$auto_crop) {
            frame_lims <-
                frame_limits(spe,
                    sampleid = input$sample,
                    image_id = input$imageid
                )
            img <-
                img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
        }

        suppressMessages(suppressWarnings(toWebGL(
            layout(
                ggplotly(
                    p,
                    width = 600,
                    height = 600,
                    source = "plotly_gene",
                    tooltip = c("fill", "key")
                ),
                images = list(
                    list(
                        source = raster2uri(img),
                        layer = "below",
                        xanchor = "left",
                        yanchor = "bottom",
                        xref = "x",
                        yref = "y",
                        sizing = "stretch",
                        x = 0,
                        y = -nrow(img),
                        sizex = ncol(img),
                        sizey = nrow(img),
                        opacity = 0.8
                    )
                ),
                dragmode = "select"
            )
        )))
    })






    observeEvent(input$update_manual_ann, {
        event.data <-
            event_data("plotly_selected", source = "plotly_histology")
        if (!is.null(event.data)) {
            isolate({
                ## Now update with the ManualAnnotation input
                rv$ManualAnnotation[spe$key %in% event.data$key] <-
                    input$label_manual_ann
            })
        }
    })

    output$click <- renderPrint({
        event.data <-
            event_data("plotly_click", source = "plotly_histology")
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a manual annotation appear here (double-click to clear)"
            )
        } else {
            isolate({
                ## Now update with the ManualAnnotation input
                if (input$label_click) {
                    rv$ManualAnnotation[spe$key %in% event.data$key] <-
                        input$label_manual_ann
                }
            })
            return(event.data$key)
        }
    })

    observeEvent(input$update_manual_ann_gene, {
        if (!is.null(input$gene_plotly_cluster_subset)) {
            event.data <- event_data("plotly_selected", source = "plotly_gene")
        } else {
            event.data <- NULL
        }
        if (!is.null(event.data)) {
            ## Prepare the data
            spe_sub <- spe[, spe$key %in% event.data$key]
            d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)

            #   Grab any continuous colData columns
            cont_cols <- as.matrix(
                colData(spe_sub)[
                    , input$geneid[input$geneid %in% colnames(colData(spe_sub))],
                    drop = FALSE
                ]
            )

            #   Get the integer indices of each gene in the SpatialExperiment, since we
            #   aren't guaranteed that rownames are gene names
            remaining_geneid <- input$geneid[!(input$geneid %in% colnames(colData(spe_sub)))]
            valid_gene_indices <- unique(
                c(
                    match(remaining_geneid, rowData(spe_sub)$gene_search),
                    match(remaining_geneid, rownames(spe_sub))
                )
            )
            valid_gene_indices <- valid_gene_indices[!is.na(valid_gene_indices)]

            #   Grab any genes
            gene_cols <- t(
                as.matrix(assays(spe_sub[valid_gene_indices, ])[[input$assayname]])
            )

            #   Combine into one matrix where rows are genes and columns are continuous
            #   features
            cont_matrix <- cbind(cont_cols, gene_cols)

            #   Determine plot and legend titles
            if (ncol(cont_matrix) == 1) {
                d$COUNT <- cont_matrix[, 1]
            } else {
                if (input$multi_gene_method == "z_score") {
                    d$COUNT <- multi_gene_z_score(cont_matrix)
                } else if (input$multi_gene_method == "sparsity") {
                    d$COUNT <- multi_gene_sparsity(cont_matrix)
                } else { # must be 'pca'
                    d$COUNT <- multi_gene_pca(cont_matrix)
                }
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the ManualAnnotation input
                rv$ManualAnnotation[spe$key %in% d$key[!is.na(d$COUNT)]] <-
                    input$label_manual_ann_gene
            })
        }
    })

    output$click_gene <- renderPrint({
        if (!is.null(input$gene_plotly_cluster_subset)) {
            event.data <- event_data("plotly_click", source = "plotly_gene")
        } else {
            event.data <- NULL
        }
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a manual annotation appear here (double-click to clear)"
            )
        } else {
            ## Prepare the data
            spe_sub <- spe[, spe$key %in% event.data$key]
            d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)

            #   Grab any continuous colData columns
            cont_cols <- as.matrix(
                colData(spe_sub)[
                    , input$geneid[input$geneid %in% colnames(colData(spe_sub))],
                    drop = FALSE
                ]
            )

            #   Get the integer indices of each gene in the SpatialExperiment, since we
            #   aren't guaranteed that rownames are gene names
            remaining_geneid <- input$geneid[!(input$geneid %in% colnames(colData(spe_sub)))]
            valid_gene_indices <- unique(
                c(
                    match(remaining_geneid, rowData(spe_sub)$gene_search),
                    match(remaining_geneid, rownames(spe_sub))
                )
            )
            valid_gene_indices <- valid_gene_indices[!is.na(valid_gene_indices)]

            #   Grab any genes
            gene_cols <- t(
                as.matrix(assays(spe_sub[valid_gene_indices, ])[[input$assayname]])
            )

            #   Combine into one matrix where rows are genes and columns are continuous
            #   features
            cont_matrix <- cbind(cont_cols, gene_cols)

            #   Determine plot and legend titles
            if (ncol(cont_matrix) == 1) {
                d$COUNT <- cont_matrix[, 1]
            } else {
                if (input$multi_gene_method == "z_score") {
                    d$COUNT <- multi_gene_z_score(cont_matrix)
                } else if (input$multi_gene_method == "sparsity") {
                    d$COUNT <- multi_gene_sparsity(cont_matrix)
                } else { # must be 'pca'
                    d$COUNT <- multi_gene_pca(cont_matrix)
                }
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the ManualAnnotation input
                if (input$label_click_gene) {
                    rv$ManualAnnotation[spe$key %in% d$key[!is.na(d$COUNT)]] <-
                        input$label_manual_ann_gene
                }
            })
            return(event.data$key)
        }
    })

    ## Raw summary
    output$raw_summary <- renderPrint(print(spe),
        width = 80
    )

    ## Download results
    output$downloadData <- downloadHandler(
        filename = function() {
            gsub(":", "-", gsub(
                " ",
                "_",
                paste0("spatialLIBD_ManualAnnotation_", Sys.time(), ".csv")
            ))
        },
        content = function(file) {
            current <- data.frame(
                sample_id = spe$sample_id,
                spot_name = colnames(spe),
                ManualAnnotation = rv$ManualAnnotation,
                stringsAsFactors = FALSE
            )
            ## Keep the NAs?
            if (input$dropNA) {
                current <- subset(current, ManualAnnotation != "NA")
            }
            write.csv(current, file, row.names = FALSE)
        }
    )

    ## Upload prior results
    observeEvent(input$priorGuesses, {
        if (!is.null(input$priorGuesses)) {
            previous_work <-
                read.csv(
                    input$priorGuesses$datapath,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    na.strings = ""
                )
            ## Update the non-NA
            previous_work <-
                subset(previous_work, ManualAnnotation != "NA")
            previous_work$key <-
                paste0(
                    previous_work$spot_name,
                    "_",
                    previous_work$sample_id
                )
            m <- match(previous_work$key, spe$key)
            if (all(is.na(m))) {
                ## For backwards compatibility with older versions of spatialLIBD
                previous_work$key <-
                    paste0(
                        previous_work$sample_id,
                        "_",
                        previous_work$spot_name
                    )
                m <- match(previous_work$key, spe$key)
                if (all(is.na(m))) {
                    stop("Cannot use previous manual annotations.",
                        call. = FALSE
                    )
                }
            }
            rv$ManualAnnotation[m[!is.na(m)]] <-
                previous_work$ManualAnnotation[!is.na(m)]
        }
    })



    #####################
    ### Layer portion ###
    #####################

    output$layer_raw_summary <- renderPrint(print(sce_layer),
        width = 80
    )

    # Set the options based on the model
    observeEvent(input$layer_model, {
        if (!is.null(input$layer_model)) {
            model_i <- which(sig_genes$model_type == input$layer_model)
            updatePickerInput(
                session,
                inputId = "layer_model_test",
                choices = sort(unique(sig_genes$test[model_i])),
                selected = sort(unique(sig_genes$test[model_i]))[1],
                options = pickerOptions(liveSearch = TRUE)
            )
        }
    })

    # Set the genes based on the model test
    observeEvent(!is.null(input$layer_model) &&
        !is.null(input$layer_model_test), {
        if (!is.null(input$layer_model) &&
            !is.null(input$layer_model_test)) {
            model_test_i <-
                which(
                    sig_genes$model_type == input$layer_model &
                        sig_genes$test == input$layer_model_test
                )
            current <- input$layer_geneid
            if (is.null(current)) {
                current <- sort(rowData(sce_layer)$gene_search)[1]
            }
            new_gene <- ifelse(
                current %in% rowData(sce_layer)$gene_search[sig_genes$gene_index[model_test_i]],
                current,
                sort(rowData(sce_layer)$gene_search[sig_genes$gene_index[model_test_i]])[1]
            )
            updatePickerInput(
                session,
                inputId = "layer_geneid",
                choices =
                    sort(rowData(sce_layer)$gene_search[sig_genes$gene_index[model_test_i]]),
                selected = new_gene,
                options = pickerOptions(liveSearch = TRUE)
            )
        }
    })

    ## layer static plots
    observeEvent(input$layer_which_dim, {
        updateNumericInput(
            inputId = "layer_reduced_dim_ncomponents",
            value = 2,
            max = ncol(reducedDim(
                sce_layer, input$layer_which_dim
            ))
        )
    })

    static_layer_reducedDim <- reactive({
        p <- scater::plotReducedDim(
            sce_layer,
            dimred = input$layer_which_dim,
            colour_by = input$layer_which_dim_color,
            theme_size = input$layer_reduced_dim_theme_size,
            point_size = input$layer_reduced_dim_point_size,
            ncomponents = input$layer_reduced_dim_ncomponents,
            label_format = c("%s %02i", " (%i%%)")
        )
        if (input$layer_which_dim_color %in% c(
            "layer_guess",
            "layer_guess_reordered"
        )) {
            p <-
                p + ggplot2::scale_color_manual(
                    values = cols_layers_paper(),
                    name = "Layer"
                )
        } else if (input$layer_which_dim_color %in% "layer_guess_reordered_short") {
            p <-
                p + ggplot2::scale_color_manual(
                    values = cols_layers_paper_short(),
                    name = "Layer"
                )
        } else if (paste0(input$layer_which_dim_color, "_colors") %in% colnames(colData(sce_layer))) {
            p <-
                p + ggplot2::scale_color_manual(
                    values = colData(sce_layer)[[paste0(input$layer_which_dim_color, "_colors")]],
                    name = input$layer_which_dim_color,
                    labels = levels(colData(sce_layer)[[input$layer_which_dim_color]]),
                    breaks = levels(colData(sce_layer)[[input$layer_which_dim_color]])
                )
        }
        return(p)
    })


    static_layer_boxplot_i <- reactive({
        which(
            sig_genes$gene_index == which(rowData(sce_layer)$gene_search == input$layer_geneid) &
                sig_genes$test == input$layer_model_test &
                sig_genes$model_type == input$layer_model
        )
    })
    static_layer_boxplot <- reactive({
        i <- static_layer_boxplot_i()
        if (length(i) > 0) {
            set.seed(20200206)
        } else {
            return(NULL)
        }
        layer_boxplot(
            i = i,
            sig_genes = sig_genes,
            short_title = input$layer_box_shortitle,
            sce_layer = sce_layer,
            col_bkg_box = ifelse(
                input$layer_boxcolor %in% c("viridis", "paper"),
                "grey80",
                "grey90"
            ),
            col_bkg_point = ifelse(
                input$layer_boxcolor %in% c("viridis", "paper"),
                "grey90",
                "grey60"
            ),
            col_low_box = ifelse(
                input$layer_boxcolor == "viridis",
                viridis(4)[2],
                ifelse(
                    input$layer_boxcolor == "paper",
                    "palegreen3",
                    "skyblue2"
                )
            ),
            col_low_point = ifelse(
                input$layer_boxcolor == "viridis",
                viridis(4)[1],
                ifelse(
                    input$layer_boxcolor == "paper",
                    "springgreen2",
                    "royalblue3"
                )
            ),
            col_high_box = ifelse(
                input$layer_boxcolor == "viridis",
                viridis(4)[3],
                ifelse(
                    input$layer_boxcolor == "paper",
                    "darkorange2",
                    "tomato2"
                )
            ),
            col_high_point = ifelse(
                input$layer_boxcolor == "viridis",
                viridis(4)[4],
                ifelse(
                    input$layer_boxcolor == "paper",
                    "orange1",
                    "firebrick4"
                )
            ),
            cex = input$layer_box_cex,
            group_var = default_cluster,
            assayname = input$layer_model_assayname
        )
    })


    static_layer_gene_set_enrichment <- reactive({
        if (!is.null(input$geneSet)) {
            gene_list <-
                read.csv(
                    input$geneSet$datapath,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    check.names = FALSE,
                    row.names = NULL
                )
        } else {
            ## Provide a working example when there's no data
            asd_sfari <- utils::read.csv(
                system.file(
                    "extdata",
                    "SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv",
                    package = "spatialLIBD"
                ),
                as.is = TRUE
            )
            asd_sfari_geneList <- list(
                Gene_SFARI_all = asd_sfari$ensembl.id,
                Gene_SFARI_high = asd_sfari$ensembl.id[asd_sfari$gene.score < 3],
                Gene_SFARI_syndromic = asd_sfari$ensembl.id[asd_sfari$syndromic == 1]
            )
            gene_list <- asd_sfari_geneList
        }

        ## Run the enrichment
        gene_set_enrichment(
            gene_list,
            input$layer_gene_fdrcut,
            modeling_results,
            input$layer_model,
            reverse = ifelse(
                input$layer_model != "anova",
                input$enrichment_reverse,
                FALSE
            )
        )
    })

    static_layer_gene_set_enrichment_plot <- reactive({
        gene_set_enrichment_plot(static_layer_gene_set_enrichment())
    })


    static_layer_external_tstat <- reactive({
        if (!is.null(input$externalTstat)) {
            input_stat <-
                read.csv(
                    input$externalTstat$datapath,
                    header = TRUE,
                    stringsAsFactors = FALSE,
                    na.strings = "",
                    check.names = FALSE,
                    row.names = 1
                )
        } else {
            ## Provide a working example when there's no data
            input_stat <-
                spatialLIBD::tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer
        }

        ## Compute the correlations
        layer_stat_cor(input_stat, modeling_results, input$layer_model)
    })

    static_layer_external_tstat_plot <- reactive({
        layer_stat_cor_plot(
            static_layer_external_tstat(),
            max(c(0.1, input$layer_tstat_max))
        )
    })

    ## layer download PDF buttons
    output$layer_downloadReducedDim <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_reducedDim_",
                    input$layer_which_dim,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 9 + input$layer_reduced_dim_ncomponents %/% 2,
                width = 10 + input$layer_reduced_dim_ncomponents %/% 2
            )
            print(static_layer_reducedDim())
            dev.off()
        }
    )

    output$layer_downloadBoxplot <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_boxplot_",
                    input$layer_model,
                    "_",
                    input$layer_model_test,
                    "_",
                    input$layer_geneid,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 8
            )
            i <- static_layer_boxplot_i()
            if (length(i) > 0) {
                set.seed(20200206)
            }
            layer_boxplot(
                i = i,
                sig_genes = sig_genes,
                short_title = input$layer_box_shortitle,
                sce_layer = sce_layer,
                col_bkg_box = ifelse(
                    input$layer_boxcolor %in% c("viridis", "paper"),
                    "grey80",
                    "grey90"
                ),
                col_bkg_point = ifelse(
                    input$layer_boxcolor %in% c("viridis", "paper"),
                    "grey90",
                    "grey60"
                ),
                col_low_box = ifelse(
                    input$layer_boxcolor == "viridis",
                    viridis(4)[2],
                    ifelse(
                        input$layer_boxcolor == "paper",
                        "palegreen3",
                        "skyblue2"
                    )
                ),
                col_low_point = ifelse(
                    input$layer_boxcolor == "viridis",
                    viridis(4)[1],
                    ifelse(
                        input$layer_boxcolor == "paper",
                        "springgreen2",
                        "royalblue3"
                    )
                ),
                col_high_box = ifelse(
                    input$layer_boxcolor == "viridis",
                    viridis(4)[3],
                    ifelse(
                        input$layer_boxcolor == "paper",
                        "darkorange2",
                        "tomato2"
                    )
                ),
                col_high_point = ifelse(
                    input$layer_boxcolor == "viridis",
                    viridis(4)[4],
                    ifelse(
                        input$layer_boxcolor == "paper",
                        "orange1",
                        "firebrick4"
                    )
                ),
                cex = input$layer_box_cex,
                group_var = input$layer_which_dim_color,
                assayname = input$layer_model_assayname
            )
            dev.off()
        }
    )

    output$layer_downloadGeneSet <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_GeneSetEnrichment_",
                    input$layer_model,
                    "_fdrcut",
                    input$layer_gene_fdrcut,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 8
            )
            gene_set_enrichment_plot(static_layer_gene_set_enrichment())
            dev.off()
        }
    )

    output$layer_downloadTstatCor <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_TstatCorHeatmap_",
                    input$layer_model,
                    "_",
                    Sys.time(),
                    ".pdf"
                )
            )
        },
        content = function(file) {
            pdf(
                file = file,
                useDingbats = FALSE,
                height = 8,
                width = 12
            )
            layer_stat_cor_plot(
                static_layer_external_tstat(),
                max(c(0.1, input$layer_tstat_max))
            )
            dev.off()
        }
    )

    ## layer plots
    output$layer_reduced_dim <- renderPlot(
        {
            print(static_layer_reducedDim())
        },
        width = function() {
            600 + 100 * input$layer_reduced_dim_ncomponents %/% 2
        },
        height = function() {
            500 + 100 * input$layer_reduced_dim_ncomponents %/% 2
        }
    )

    output$layer_boxplot <- renderPlot(
        {
            static_layer_boxplot()
        },
        width = 600,
        height = 600
    )

    output$layer_gene_set_plot <- renderPlot(
        {
            static_layer_gene_set_enrichment_plot()
        },
        width = 600,
        height = 600
    )

    output$layer_tstat_cor_plot <- renderPlot(
        {
            static_layer_external_tstat_plot()
        },
        width = 800,
        height = 600
    )


    ## interactive tables
    layer_model_table_reactive <- reactive({
        as.data.frame(subset(
            sig_genes[, seq_len(which(colnames(sig_genes) == "ensembl"))],
            model_type == input$layer_model &
                ensembl == gsub(".*; ", "", input$layer_geneid)
        ))
    })

    layer_model_table_full_reactive <- reactive({
        as.data.frame(
            subset(
                sig_genes[, seq_len(which(colnames(sig_genes) == "ensembl"))],
                model_type == input$layer_model &
                    test == input$layer_model_test
            )
        )
    })

    output$layer_model_table <- DT::renderDT(
        layer_model_table_reactive(),
        style = "bootstrap",
        rownames = FALSE,
        filter = "top",
        options = list(
            columnDefs = list(list(
                className = "dt-center", targets = 1
            )),
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50, 100),
            order = list(list(0, "asc"))
        )
    )

    output$layer_model_table_full <- DT::renderDT(
        layer_model_table_full_reactive(),
        style = "bootstrap",
        rownames = FALSE,
        filter = "top",
        options = list(
            columnDefs = list(list(
                className = "dt-center", targets = 1
            )),
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50, 100),
            order = list(list(0, "asc"))
        )
    )

    output$layer_gene_set_table <- DT::renderDT(
        static_layer_gene_set_enrichment(),
        style = "bootstrap",
        rownames = FALSE,
        filter = "top",
        options = list(
            columnDefs = list(list(
                className = "dt-center", targets = 1
            )),
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50, 100),
            order = list(list(0, "desc"))
        )
    )

    output$layer_tstat_cor_table <- DT::renderDT(
        static_layer_external_tstat(),
        style = "bootstrap",
        rownames = TRUE,
        filter = "top",
        options = list(
            columnDefs = list(list(
                className = "dt-center", targets = 1
            )),
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50, 100)
        )
    )

    ## Download tables
    output$layer_downloadModelTable <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_",
                    input$layer_model,
                    "_",
                    input$layer_model_test,
                    "_",
                    input$layer_geneid,
                    "_",
                    Sys.time(),
                    ".csv"
                )
            )
        },
        content = function(file) {
            write.csv(
                layer_model_table_reactive(),
                file = file,
                quote = FALSE,
                row.names = FALSE
            )
        }
    )

    output$layer_downloadModelTable_full <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_",
                    input$layer_model,
                    "_",
                    input$layer_model_test,
                    "_",
                    Sys.time(),
                    ".csv"
                )
            )
        },
        content = function(file) {
            write.csv(
                layer_model_table_full_reactive(),
                file = file,
                quote = FALSE,
                row.names = FALSE
            )
        }
    )

    output$layer_downloadGeneSetTable <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_GeneSetEnrichment_",
                    input$layer_model,
                    "_fdrcut",
                    input$layer_gene_fdrcut,
                    "_",
                    Sys.time(),
                    ".csv"
                )
            )
        },
        content = function(file) {
            write.csv(
                static_layer_gene_set_enrichment(),
                file = file,
                quote = FALSE,
                row.names = FALSE
            )
        }
    )

    output$layer_downloadTstatCorTable <- downloadHandler(
        filename = function() {
            gsub(
                " ",
                "_",
                paste0(
                    "spatialLIBD_layer_TstatCor_",
                    input$layer_model,
                    "_",
                    Sys.time(),
                    ".csv"
                )
            )
        },
        content = function(file) {
            write.csv(
                static_layer_external_tstat(),
                file = file,
                quote = FALSE,
                row.names = TRUE
            )
        }
    )



    ## Reproducibility info
    output$session_info <-
        renderPrint(sessioninfo::session_info(), width = 120)
}
