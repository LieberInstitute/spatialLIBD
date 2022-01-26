#' @import shiny
#' @import SingleCellExperiment
#' @import ggplot2
#' @import grid
#' @import Polychrome
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
    COUNT <- model_type <- ensembl <- key <- ManualAnnotation <- NULL

    ## Get options
    spe <- golem::get_golem_options("spe")
    sce_layer <- golem::get_golem_options("sce_layer")
    modeling_results <- golem::get_golem_options("modeling_results")
    sig_genes <- golem::get_golem_options("sig_genes")


    # List the first level callModules here

    ## Global variables needed throughout the app
    rv <- reactiveValues(ManualAnnotation = rep("NA", ncol(spe)))

    ## From /dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/rda_scran/clust_10x_layer_maynard_martinowich.Rdata
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

    cluster_colors <- reactive({
        colors <- NULL
        if (input$cluster %in% c("Maynard", "Martinowich")) {
            colors <- cols_layers_martinowich
        } else if (input$cluster == "ManualAnnotation") {
            colors <-
                Polychrome::palette36.colors(length(unique(rv$ManualAnnotation)))
            names(colors) <- unique(rv$ManualAnnotation)
        } else if (input$cluster %in% c("layer_guess", "layer_guess_reordered")) {
            colors <- cols_layers_paper()
        } else if (input$cluster %in% c("layer_guess_reordered_short", "spatialLIBD")) {
            colors <- cols_layers_paper_short()
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
            viridis(21, option = input$genecolor, direction = ifelse(input$genecolor_direction, 1, -1))
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
                    p_no_spots, #+ ggplot2::geom_point(colour = "black", fill = NA, shape = 21),
                    p_no_spatial + ggplot2::theme(legend.position = "none")
                ), nrow = 1, ncol = 2
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
                image_id = isolate(input$imageid),
                alpha = isolate(input$alphalevel),
                sample_order = isolate(input$grid_samples),
                point_size = isolate(input$pointsize),
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
            assayname = input$assayname,
            minCount = input$minCount,
            cont_colors = cont_colors(),
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize
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
                    p_no_spots, #+ ggplot2::geom_point(colour = "black", fill = NA, shape = 21),
                    p_no_spatial + ggplot2::theme(legend.position = "none")
                ), nrow = 1, ncol = 2
            )
        }
    })

    static_gene_grid <- reactive({
        input$gene_grid_update

        plots <-
            vis_grid_gene(
                spe,
                geneid = isolate(input$geneid),
                assayname = isolate(input$assayname),
                minCount = isolate(input$minCount),
                return_plots = TRUE,
                cont_colors = isolate(cont_colors()),
                image_id = isolate(input$imageid),
                alpha = isolate(input$alphalevel),
                point_size = isolate(input$pointsize),
                sample_order = isolate(input$gene_grid_samples)
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
                    input$geneid,
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
                    input$geneid,
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
        width = function() 600 * ifelse(input$side_by_side_histology, 2, 1),
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
        width = function() 600 * ifelse(input$side_by_side_gene, 2, 1),
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
        img <- SpatialExperiment::imgRaster(spe, sample_id = sampleid, image_id = input$imageid)

        ## From vis_gene() in global.R
        d <- as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$sample_id == sampleid, ], optional = TRUE)
        if (geneid %in% colnames(d)) {
            d$COUNT <- d[[geneid]]
        } else {
            d$COUNT <-
                assays(spe)[[assayname]][which(rowData(spe)$gene_search == geneid), spe$sample_id == sampleid]
        }
        d$COUNT[d$COUNT <= minCount] <- NA

        ## Add the reduced dims
        if (reduced_name != "") {
            red_dims <- reducedDim(spe, reduced_name)[spe$sample_id == sampleid, ]
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
            title = paste(
                sampleid,
                clustervar,
                geneid,
                if (!geneid %in% colnames(colData(spe))) {
                    assayname
                } else {
                    NULL
                },
                "min >",
                minCount
            ),
            image_id = input$imageid,
            alpha = input$alphalevel,
            point_size = input$pointsize
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
            point_size = input$pointsize
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
                    stroke = 0.25
                ) +
                scale_fill_manual(values = get_colors(colors, colData(spe)[[clustervar]][spe$sample_id == sampleid])) +
                guides(fill = FALSE) +
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
                    stroke = 0.25
                )
        } else {
            p_dim <- p_dim_gene <- ggplot(d_key, aes(key = key))
        }

        p_dim_gene <- p_dim_gene + scale_fill_gradientn(
            colors = cont_colors(),
            na.value = c("black" = "#0000002D"),
            guide = FALSE
        ) + scale_color_gradientn(
            colors = cont_colors(),
            na.value = c("black" = "#0000002D"),
            guide = FALSE
        )


        p_dim_gene <- p_dim_gene +
            labs(fill = NULL) +
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
        plotly_clus <- layout(
            ggplotly(
                p_clus,
                width = 600 * 2,
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
            ggplotly(p_gene,
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

        plotly_dim <- layout(ggplotly(p_dim,
            source = "plotly_histology",
            tooltip = c("fill", "key")
        ))

        plotly_dim_gene <- layout(ggplotly(p_dim_gene,
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
                subplot(
                    plotly_dim_gene,
                    plotly_dim,
                    nrows = 1,
                    shareX = TRUE,
                    shareY = TRUE,
                    which_layout = 2
                ),
                nrows = 2,
                shareX = FALSE,
                shareY = FALSE,
                which_layout = 1
            ),
            legend = list(tracegroupgap = 0, x = 1.1)
        )

        ## Restore some axis titles for the reduced dim plot
        plotly_merged$x$layout$xaxis3$title <-
            plotly_merged$x$layout$xaxis4$title <-
            plotly_dim$x$layout$xaxis$title
        plotly_merged$x$layout$yaxis2$title <-
            plotly_dim$x$layout$yaxis$title

        ## Make the linked (client-side) plot
        suppressMessages(suppressWarnings(toWebGL(highlight(plotly_merged,
            on = "plotly_selected",
            off = "plotly_deselect"
        ))))
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
            cluster_opts <- rv$ManualAnnotation %in% input$gene_plotly_cluster_subset
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
                assayname = input$assayname,
                minCount = input$minCount,
                spatial = FALSE,
                cont_colors = cont_colors(),
                image_id = input$imageid,
                alpha = input$alphalevel,
                point_size = input$pointsize
            )

        ## Read in the histology image
        img <- SpatialExperiment::imgRaster(spe, sample_id = input$sample, image_id = input$imageid)

        suppressMessages(suppressWarnings(toWebGL(layout(
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
        ))))
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
            d <- as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$key %in% event.data$key, ], optional = TRUE)
            if (input$geneid %in% colnames(d)) {
                d$COUNT <- d[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(spe)[[input$assayname]][which(rowData(spe)$gene_search == input$geneid), spe$key %in% event.data$key]
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
            d <- as.data.frame(cbind(colData(spe), SpatialExperiment::spatialCoords(spe))[spe$key %in% event.data$key, ], optional = TRUE)
            if (input$geneid %in% colnames(d)) {
                d$COUNT <- d[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(spe)[[input$assayname]][which(rowData(spe)$gene_search == input$geneid), spe$key %in% event.data$key]
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
            previous_work <- subset(previous_work, ManualAnnotation != "NA")
            previous_work$key <-
                paste0(
                    previous_work$sample_id,
                    "_",
                    previous_work$spot_name
                )
            m <- match(previous_work$key, spe$key)
            rv$ManualAnnotation[m[!is.na(m)]] <- previous_work$ManualAnnotation[!is.na(m)]
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
            sig_genes_sub <- subset(sig_genes, model_type == input$layer_model)
            updateSelectInput(
                session,
                inputId = "layer_model_test",
                choices = sort(unique(sig_genes_sub$test)),
                selected = sort(unique(sig_genes_sub$test))[1]
            )
        }
    })

    ## layer static plots
    static_layer_reducedDim <- reactive({
        p <- scater::plotReducedDim(
            sce_layer,
            dimred = input$layer_which_dim,
            colour_by = input$layer_which_dim_color,
            theme_size = 30,
            point_size = 7
        )
        if (input$layer_which_dim_color %in% c(
            "layer_guess",
            "layer_guess_reordered"
        )) {
            p <-
                p + ggplot2::scale_fill_manual(
                    values = cols_layers_paper(),
                    name = "Layer"
                )
        } else if (input$layer_which_dim_color %in% "layer_guess_reordered_short") {
            p <-
                p + ggplot2::scale_fill_manual(
                    values = cols_layers_paper_short(),
                    name = "Layer"
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
            cex = 2.7
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
            input$layer_model
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
                height = 8,
                width = 9
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
                cex = 2.7
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
        width = 700,
        height = 600
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
