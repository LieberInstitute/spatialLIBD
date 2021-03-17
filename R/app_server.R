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
    COUNT <- model_type <- ensembl <- key <- NULL

    ## Get options
    spe <- golem::get_golem_options("spe")
    sce_layer <- golem::get_golem_options("sce_layer")
    modeling_results <- golem::get_golem_options("modeling_results")
    sig_genes <- golem::get_golem_options("sig_genes")
    spatial_libd_var <- golem::get_golem_options("spatial_libd_var")

    ## Rename some variables
    spe$spatialLIBD <- colData(spe)[[spatial_libd_var]]
    sce_layer$spatialLIBD <- colData(sce_layer)[[spatial_libd_var]]

    # List the first level callModules here

    ## Global variables needed throughout the app
    rv <- reactiveValues(layer = rep("NA", ncol(spe)))

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
        } else if (input$cluster == "Layer") {
            colors <-
                Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        } else if (input$cluster %in% c("layer_guess", "layer_guess_reordered")) {
            colors <- cols_layers_paper()
        } else if (input$cluster %in% c("layer_guess_reordered_short", "spatialLIBD")) {
            colors <- cols_layers_paper_short()
        }
        return(colors)
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
        if (input$cluster == "Layer") {
            spe$Layer <- rv$layer
        }
        vis_clus(
            spe,
            sampleid = input$sample,
            clustervar = input$cluster,
            colors = cluster_colors(),
            ... = paste(" with", input$cluster)
        )
    })

    static_histology_grid <- reactive({
        input$grid_update

        if (isolate(input$cluster == "Layer")) {
            spe$Layer <- rv$layer
        }
        spe_sub <-
            spe[, spe$sample_id %in% isolate(input$grid_samples)]
        plots <-
            vis_grid_clus(
                spe_sub,
                isolate(input$cluster),
                sort_clust = FALSE,
                colors = cluster_colors(),
                return_plots = TRUE,
                ... = paste(" with", isolate(input$cluster))
            )
        cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$grid_nrow),
            ncol = isolate(input$grid_ncol)
        )
    })

    static_gene <- reactive({
        vis_gene(
            spe,
            sampleid = input$sample,
            geneid = input$geneid,
            assayname = input$assayname,
            minCount = input$minCount,
            viridis = input$genecolor == "viridis"
        )
    })

    static_gene_grid <- reactive({
        input$gene_grid_update

        spe_sub <-
            spe[, spe$sample_id %in% isolate(input$gene_grid_samples)]
        plots <-
            vis_grid_gene(
                spe_sub,
                geneid = isolate(input$geneid),
                assayname = isolate(input$assayname),
                minCount = isolate(input$minCount),
                return_plots = TRUE,
                viridis = isolate(input$genecolor == "viridis")
            )
        cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$gene_grid_nrow),
            ncol = isolate(input$gene_grid_ncol)
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
                width = 9
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
                width = 8
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

    ## Clusters/Layers
    output$histology <- renderPlot(
        {
            static_histology()
        },
        width = 600,
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
        width = 600,
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


    ## Plotly versions
    output$histology_plotly <- renderPlotly({
        if (input$cluster == "Layer") {
            spe$Layer <- rv$layer
        }
        colors <- cluster_colors()

        ## Define some common arguments
        sampleid <- input$sample
        geneid <- input$geneid
        assayname <- input$assayname
        minCount <- input$minCount
        clustervar <- input$cluster
        reduced_name <- input$reduced_name
        #
        # ## Testing:
        # sampleid <- '151507'
        # geneid <- "SCGB2A2; ENSG00000110484"
        # assayname <- 'logcounts'
        # minCount <- 0
        # clustervar <- 'GraphBased'
        # colors <- get_colors(NULL, spe$GraphBased)
        # reduced_name <- 'TSNE_perplexity50'

        ## Read in the histology image
        img <- SpatialExperiment::imgRaster(spe, sample_id = input$sample)

        ## From vis_gene() in global.R
        spe_sub <- spe[, spe$sample_id == sampleid]
        d <- SpatialExperiment::spatialData(spe_sub, cd_bind = TRUE, as_df = TRUE)
        if (geneid %in% colnames(colData(spe_sub))) {
            d$COUNT <- colData(spe_sub)[[geneid]]
        } else {
            d$COUNT <-
                assays(spe_sub)[[assayname]][which(rowData(spe_sub)$gene_search == geneid), ]
        }
        d$COUNT[d$COUNT <= minCount] <- NA

        ## Add the reduced dims
        red_dims <- reducedDim(spe_sub, reduced_name)
        colnames(red_dims) <- paste(reduced_name, "dim", seq_len(2))
        d <- cbind(d, red_dims)

        ## Drop points below minCount
        d <- subset(d, !is.na(COUNT))

        ## Use client-side highlighting
        d_key <- highlight_key(d, ~key)

        ## Make the cluster plot
        p_clus <- vis_clus_p(
            spe = spe_sub,
            d = d_key,
            clustervar = clustervar,
            sampleid = sampleid,
            colors = get_colors(colors, d[, clustervar]),
            spatial = FALSE,
            title = paste(
                sampleid,
                clustervar,
                geneid,
                if (!geneid %in% colnames(colData(spe_sub))) {
                    assayname
                } else {
                    NULL
                },
                "min >",
                minCount
            )
        )

        ## Next the gene plot
        p_gene <- vis_gene_p(
            spe = spe_sub,
            d = d_key,
            sampleid = sampleid,
            spatial = FALSE,
            title = "",
            viridis = input$genecolor == "viridis"
        )

        ## Make the reduced dimensions ggplot
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
                size = 1.25,
                stroke = 0.25
            ) +
            scale_fill_manual(values = get_colors(colors, colData(spe_sub)[[clustervar]])) +
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
                size = 1.25,
                stroke = 0.25
            )


        if (input$genecolor == "viridis") {
            p_dim_gene <-
                p_dim_gene + scale_fill_gradientn(
                    colors = viridis(21),
                    na.value = c("black" = "#0000002D"),
                    guide = FALSE
                ) +
                scale_color_gradientn(
                    colors = viridis(21),
                    na.value = c("black" = "#0000002D"),
                    guide = FALSE
                )
        } else {
            p_dim_gene <- p_dim_gene + scale_fill_gradientn(
                colors = c("aquamarine4", "springgreen", "goldenrod", "red"),
                na.value = c("black" = "#0000002D"),
                guide = FALSE
            ) + scale_color_gradientn(
                colors = c("aquamarine4", "springgreen", "goldenrod", "red"),
                na.value = c("black" = "#0000002D"),
                guide = FALSE
            )
        }

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
                source = "plotly_histology"
            ),
            images = list(
                list(
                    source = raster2uri(img),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = "below",
                    sizing = "stretch"
                )
            ),
            dragmode = "select"
        )

        plotly_gene <- layout(
            ggplotly(p_gene,
                source = "plotly_histology"
            ),
            images = list(
                list(
                    source = raster2uri(img),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = "below",
                    sizing = "stretch"
                )
            ),
            dragmode = "select"
        )

        plotly_dim <- layout(ggplotly(p_dim,
            source = "plotly_histology"
        ))

        plotly_dim_gene <- layout(ggplotly(p_dim_gene,
            source = "plotly_histology"
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
        highlight(plotly_merged,
            on = "plotly_selected",
            off = "plotly_deselect"
        )
    })

    ## Set the cluster subset options
    output$gene_plotly_cluster_subset_ui <- renderUI({
        input$cluster

        if (input$cluster == "Layer") {
            cluster_opts <- unique(rv$layer)
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

        if (input$cluster == "Layer") {
            cluster_opts <- rv$layer %in% input$gene_plotly_cluster_subset
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
                viridis = input$genecolor == "viridis"
            )

        ## Read in the histology image
        img <- SpatialExperiment::imgRaster(spe, sample_id = input$sample)

        layout(
            ggplotly(
                p,
                width = 600,
                height = 600,
                source = "plotly_gene"
            ),
            images = list(
                list(
                    source = raster2uri(img),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = "below",
                    sizing = "stretch"
                )
            ),
            dragmode = "select"
        )
    })






    observeEvent(input$update_layer, {
        event.data <-
            event_data("plotly_selected", source = "plotly_histology")
        if (!is.null(event.data)) {
            isolate({
                ## Now update with the layer input
                rv$layer[spe$key %in% event.data$key] <-
                    input$label_layer
            })
        }
    })

    output$click <- renderPrint({
        event.data <-
            event_data("plotly_click", source = "plotly_histology")
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a layer guess appear here (double-click to clear)"
            )
        } else {
            isolate({
                ## Now update with the layer input
                if (input$label_click) {
                    rv$layer[spe$key %in% event.data$key] <-
                        input$label_layer
                }
            })
            return(event.data$key)
        }
    })

    observeEvent(input$update_layer_gene, {
        if (!is.null(input$gene_plotly_cluster_subset)) {
            event.data <- event_data("plotly_selected", source = "plotly_gene")
        } else {
            event.data <- NULL
        }
        if (!is.null(event.data)) {
            ## Prepare the data
            spe_sub <- spe[, spe$key %in% event.data$key]
            d <- SpatialExperiment::spatialData(spe_sub, cd_bind = TRUE, as_df = TRUE)
            if (input$geneid %in% colnames(colData(spe_sub))) {
                d$COUNT <- colData(spe_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(spe_sub)[[input$assayname]][which(rowData(spe_sub)$gene_search == input$geneid), ]
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the layer input
                rv$layer[spe$key %in% d$key[!is.na(d$COUNT)]] <-
                    input$label_layer_gene
            })
        }
    })

    output$click_gene <- renderPrint({
        if (!is.null(input$gene_plotly_cluster_subset)) {
            event.data <- event_data("plotly_selected", source = "plotly_gene")
        } else {
            event.data <- NULL
        }
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a layer guess appear here (double-click to clear)"
            )
        } else {
            ## Prepare the data
            spe_sub <- spe[, spe$key %in% event.data$key]
            d <- SpatialExperiment::spatialData(spe_sub, cd_bind = TRUE, as_df = TRUE)
            if (input$geneid %in% colnames(colData(spe_sub))) {
                d$COUNT <- colData(spe_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(spe_sub)[[input$assayname]][which(rowData(spe_sub)$gene_search == input$geneid), ]
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the layer input
                if (input$label_click_gene) {
                    rv$layer[spe$key %in% d$key[!is.na(d$COUNT)]] <-
                        input$label_layer_gene
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
                paste0("spatialLIBD_layerGuesses_", Sys.time(), ".csv")
            ))
        },
        content = function(file) {
            current <- data.frame(
                sample_id = spe$sample_id,
                spot_name = colnames(spe),
                layer = rv$layer,
                stringsAsFactors = FALSE
            )
            ## Keep the NAs?
            if (input$dropNA) {
                current <- subset(current, layer != "NA")
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
            previous_work <- subset(previous_work, layer != "NA")
            previous_work$key <-
                paste0(
                    previous_work$sample_id,
                    "_",
                    previous_work$spot_name
                )
            m <- match(previous_work$key, spe$key)
            rv$layer[m[!is.na(m)]] <- previous_work$layer[!is.na(m)]
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
