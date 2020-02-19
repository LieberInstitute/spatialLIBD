#' @import shiny
#' @import SingleCellExperiment
#' @import ggplot2
#' @import grid
#' @import Polychrome
#' @importFrom cowplot plot_grid
#' @importFrom viridisLite viridis
#' @importFrom sessioninfo session_info
#' @import plotly
#' @importFrom grDevices as.raster

app_server <- function(input, output, session) {
    ## Get options
    sce <- golem::get_golem_options('sce')
    sce_layer <- golem::get_golem_options('sce_layer')
    modeling_results <- golem::get_golem_options('modeling_results')

    # List the first level callModules here

    ## Global variables needed throughout the app
    rv <- reactiveValues(layer = rep('NA', ncol(sce)))

    # Set the max based on the assay
    observeEvent(input$assayname, {
        updateNumericInput(
            session,
            inputId = 'minCount',
            value = 0,
            min = -1,
            max = max(assays(sce)[[input$assayname]]),
            step = 1
        )
    })


    ## Static plotting functions
    static_histology <- reactive({
        colors <- NULL
        if (input$cluster %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if (input$cluster == 'Layer') {
            sce$Layer <- rv$layer
            colors <-
                Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }

        sce_image_clus(
            sce,
            sampleid = input$sample,
            clustervar = input$cluster,
            colors = colors,
            ... = paste(' with', input$cluster)
        )
    })

    static_histology_grid <- reactive({
        input$grid_update

        colors <- NULL
        if (isolate(input$cluster) %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if (isolate(input$cluster == 'Layer')) {
            sce$Layer <- rv$layer
            colors <-
                Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }
        sce_sub <-
            sce[, sce$sample_name %in% isolate(input$grid_samples)]
        plots <-
            sce_image_grid(
                sce_sub,
                colData(sce_sub)[[isolate(input$cluster)]],
                sort_clust = FALSE,
                colors = colors,
                return_plots = TRUE,
                ... = paste(' with', isolate(input$cluster))
            )
        cowplot::plot_grid(
                plotlist = plots,
                nrow = isolate(input$grid_nrow),
                ncol = isolate(input$grid_ncol)
            )
    })

    static_gene <- reactive({
        sce_image_clus_gene(
            sce,
            sampleid = input$sample,
            geneid = input$geneid,
            assayname = input$assayname,
            minCount = input$minCount,
            viridis = input$genecolor == 'viridis'
        )
    })

    static_gene_grid <- reactive({
        input$gene_grid_update

        sce_sub <-
            sce[, sce$sample_name %in% isolate(input$gene_grid_samples)]
        plots <-
            sce_image_grid_gene(
                sce_sub,
                geneid = isolate(input$geneid),
                assayname = isolate(input$assayname),
                minCount = isolate(input$minCount),
                return_plots = TRUE,
                viridis = isolate(input$genecolor == 'viridis')
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
                ' ',
                '_',
                paste0(
                    'spatialLIBD_static_cluster_',
                    input$cluster,
                    '_',
                    input$sample,
                    '_',
                    Sys.time(),
                    '.pdf'
                )
            )
        },
        content = function(file) {
            pdf(file = file, useDingbats = FALSE, height = 8, width = 9)
            print(static_histology())
            dev.off()
        }
    )

    output$downloadPlotHistologyGrid <- downloadHandler(
        filename = function() {
            gsub(
                ' ',
                '_',
                paste0(
                    'spatialLIBD_static_cluster_grid_',
                    input$cluster,
                    '_',
                    paste0(input$grid_samples, collapse = '_'),
                    '_',
                    Sys.time(),
                    '.pdf'
                )
            )
        },
        content = function(file) {
            pdf(file = file, useDingbats = FALSE, height = 8 * isolate(input$grid_nrow), width = 9 * isolate(input$grid_ncol))
            print(static_histology_grid())
            dev.off()
        }
    )

    output$downloadPlotGene <- downloadHandler(
        filename = function() {
            gsub(
                ' ',
                '_',
                paste0(
                    'spatialLIBD_static_gene_',
                    input$geneid,
                    '_',
                    input$sample,
                    '_',
                    Sys.time(),
                    '.pdf'
                )
            )
        },
        content = function(file) {
            pdf(file = file, useDingbats = FALSE, height = 8, width = 9)
            print(static_gene())
            dev.off()
        }
    )

    output$downloadPlotGeneGrid <- downloadHandler(
        filename = function() {
            gsub(
                ' ',
                '_',
                paste0(
                    'spatialLIBD_static_gene_grid_',
                    input$geneid,
                    '_',
                    paste0(input$grid_samples, collapse = '_'),
                    '_',
                    Sys.time(),
                    '.pdf'
                )
            )
        },
        content = function(file) {
            pdf(file = file, useDingbats = FALSE, height = 8 * isolate(input$gene_grid_nrow), width = 9 * isolate(input$gene_grid_ncol))
            print(static_gene_grid())
            dev.off()
        }
    )

    ## Clusters/Layers
    output$histology <- renderPlot({
        static_histology()
    }, width = 600, height = 600)


    output$grid_static <- renderUI({
        input$grid_update

        plotOutput(
            'histology_grid',
            width = 600 * isolate(input$grid_ncol),
            height = 600 * isolate(input$grid_nrow)
        )
    })

    output$histology_grid <- renderPlot({
        print(static_histology_grid())
    }, width = 'auto', height = 'auto')


    output$gene <- renderPlot({
        static_gene()
    }, width = 600, height = 600)


    output$gene_grid_static <- renderUI({
        input$gene_grid_update

        plotOutput(
            'gene_grid',
            width = 600 * isolate(input$gene_grid_ncol),
            height = 600 * isolate(input$gene_grid_nrow)
        )
    })

    output$gene_grid <- renderPlot({
        print(static_gene_grid())
    }, width = 'auto', height = 'auto')


    ## Plotly versions
    output$histology_plotly <- renderPlotly({
        colors <- NULL
        if (input$cluster %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if (input$cluster == 'Layer') {
            sce$Layer <- rv$layer
            colors <-
                Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }

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
        # clustervar <- 'Cluster10X'
        # colors <- get_colors(NULL, sce$Cluster10X)
        # reduced_name <- 'TSNE_perplexity50'

        ## Read in the histology image
        pen <-
            png::readPNG(file.path(
                resourcePaths()['imagedata'],
                sampleid,
                'tissue_lowres_image.png'
            ))


        ## From sce_image_clus_gene() in global.R
        sce_sub <- sce[, sce$sample_name == sampleid]
        d <- as.data.frame(colData(sce_sub))
        if (geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[assayname]][which(rowData(sce_sub)$gene_search == geneid), ]
        }
        d$COUNT[d$COUNT <= minCount] <- NA

        ## Add the reduced dims
        red_dims <- reducedDim(sce_sub, reduced_name)
        colnames(red_dims) <- paste(reduced_name, 'dim', 1:2)
        d <- cbind(d, red_dims)

        ## Drop points below minCount
        d <- subset(d, !is.na(COUNT))

        ## Use client-side highlighting
        d_key <- highlight_key(d, ~ key)

        ## Make the cluster plot
        p_clus <-  sce_image_clus_p(
            sce = sce_sub,
            d = d_key,
            clustervar = clustervar,
            sampleid = sampleid,
            colors = get_colors(colors, d[, clustervar]),
            spatial = FALSE,
            title = paste(
                sampleid,
                'with',
                clustervar,
                '-',
                geneid,
                if (!geneid %in% colnames(colData(sce_sub)))
                    assayname,
                'min Count: >',
                minCount
            )
        )

        ## Next the gene plot
        p_gene <- sce_image_clus_gene_p(
            sce = sce_sub,
            d = d_key,
            sampleid = sampleid,
            spatial = FALSE,
            title = "",
            assayname = assayname,
            viridis = input$genecolor == 'viridis'
        )

        ## Make the reduced dimensions ggplot
        p_dim <- ggplot(d_key,
            aes(
                x = !!sym(colnames(red_dims)[1]),
                y = !!sym(colnames(red_dims)[2]),
                fill = factor(!!sym(clustervar)),
                key =  key
            )) +
            geom_point(shape = 21,
                size = 1.25,
                stroke = 0.25) +
            scale_fill_manual(values = get_colors(colors, colData(sce_sub)[[clustervar]])) +
            guides(fill = FALSE) +
            ggtitle("") +
            theme_set(theme_bw(base_size = 10)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )

        p_dim_gene <- ggplot(d_key,
            aes(
                x = !!sym(colnames(red_dims)[1]),
                y = !!sym(colnames(red_dims)[2]),
                fill = COUNT,
                color = COUNT,
                key =  key
            )) + geom_point(shape = 21,
                size = 1.25,
                stroke = 0.25)


        if (input$genecolor == 'viridis') {
            p_dim_gene <-
                p_dim_gene + scale_fill_gradientn(
                    colors = viridis(21),
                    na.value = c('black' = '#0000002D'),
                    guide = FALSE
                ) +
                scale_color_gradientn(
                    colors = viridis(21),
                    na.value = c('black' = '#0000002D'),
                    guide = FALSE
                )
        } else {
            p_dim_gene <- p_dim_gene +  scale_fill_gradientn(
                colors = c('aquamarine4', 'springgreen', 'goldenrod', 'red'),
                na.value = c('black' = '#0000002D'),
                guide = FALSE
            ) + scale_color_gradientn(
                colors = c('aquamarine4', 'springgreen', 'goldenrod', 'red'),
                na.value = c('black' = '#0000002D'),
                guide = FALSE
            )
        }

        p_dim_gene <- p_dim_gene +
            labs(fill = assayname) +
            ggtitle("") +
            theme_set(theme_bw(base_size = 10)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )

        ## Make the plotly objects with histology in the background
        plotly_clus <- layout(
            ggplotly(
                p_clus,
                width = 600,
                height = 600,
                source = 'plotly_histology'
            ),
            images = list(
                list(
                    source = raster2uri(as.raster(pen)),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = 'below',
                    sizing = 'stretch'
                )
            ),
            dragmode = 'select'
        )

        plotly_gene <- layout(
            ggplotly(
                p_gene,
                width = 600,
                height = 600,
                source = 'plotly_histology'
            ),
            images = list(
                list(
                    source = raster2uri(as.raster(pen)),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = 'below',
                    sizing = 'stretch'
                )
            ),
            dragmode = 'select'
        )

        plotly_dim <- layout(ggplotly(
            p_dim,
            width = 600,
            height = 600,
            source = 'plotly_histology'
        ))

        plotly_dim_gene <- layout(ggplotly(
            p_dim_gene,
            width = 600,
            height = 600,
            source = 'plotly_histology'
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
            width = 600 * 2,
            height = 600 * 2,
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
            on = 'plotly_selected',
            off = 'plotly_deselect')
    })

    output$histology_plotly_gene <- renderPlotly({
        event.data <-
            event_data('plotly_selected', source = 'plotly_histology')
        if (is.null(event.data))
            return(NULL)

        ## Find which points were selected
        sce_sub <- sce[, sce$key %in% event.data$key]

        d <- as.data.frame(colData(sce_sub))
        if (input$geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[input$geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[input$assayname]][which(rowData(sce_sub)$gene_search == input$geneid), ]
        }
        d$COUNT[d$COUNT <= input$minCount] <- NA
        p <-
            ggplot(d, aes(x = COUNT)) + geom_density() + ggtitle(input$geneid) + xlab(ifelse(
                !input$geneid %in% colnames(colData(sce_sub)),
                input$assayname,
                input$geneid
            ))
        ggplotly(p)
    })

    ## Set the cluster subset options
    output$gene_plotly_cluster_subset_ui <- renderUI({
        input$clusters

        if (input$cluster == 'Layer') {
            cluster_opts <- unique(rv$layer)
        } else {
            cluster_opts <- unique(colData(sce)[[input$cluster]])
        }
        checkboxGroupInput(
            'gene_plotly_cluster_subset',
            label = 'Select clusters to show in this plot',
            choices = sort(cluster_opts),
            selected = cluster_opts,
            inline = TRUE
        )
    })

    output$gene_plotly <- renderPlotly({
        if (is.null(input$gene_plotly_cluster_subset))
            return(NULL)

        pen <-
            png::readPNG(file.path(
                resourcePaths()['imagedata'],
                input$sample,
                'tissue_lowres_image.png'
            ))
        if (input$cluster == 'Layer') {
            cluster_opts <- rv$layer %in% input$gene_plotly_cluster_subset
        } else {
            cluster_opts <-
                as.character(colData(sce)[[input$cluster]]) %in% input$gene_plotly_cluster_subset
        }

        p <-
            sce_image_clus_gene(
                sce[, cluster_opts],
                sampleid = input$sample,
                geneid = input$geneid,
                assayname = input$assayname,
                minCount = input$minCount,
                spatial = FALSE,
                viridis = input$genecolor == 'viridis'
            )
        layout(
            ggplotly(
                p,
                width = 600,
                height = 600,
                source = 'plotly_gene'
            ),
            images = list(
                list(
                    source = raster2uri(as.raster(pen)),
                    xref = "paper",
                    yref = "paper",
                    x = 0,
                    y = 0,
                    sizex = 1,
                    sizey = 1,
                    xanchor = "left",
                    yanchor = "bottom",
                    opacity = 1,
                    layer = 'below',
                    sizing = 'stretch'
                )
            ),
            dragmode = 'select'
        )
    })


    output$gene_plotly_clusters <- renderPlotly({
        event.data <- event_data('plotly_selected', source = 'plotly_gene')
        if (is.null(event.data))
            return(NULL)

        ## Prepare the data
        sce$Layer <- rv$layer
        sce_sub <- sce[, sce$key %in% event.data$key]
        d <- as.data.frame(colData(sce_sub))
        if (input$geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[input$geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[input$assayname]][which(rowData(sce_sub)$gene_search == input$geneid),]
        }
        d$COUNT[d$COUNT <= input$minCount] <- NA

        ## Plot the cluster frequency
        p <-
            ggplot(subset(d, !is.na(COUNT)), aes(x = !!sym(input$cluster))) + geom_bar() + ggtitle(input$cluster)
        ggplotly(p)
    })






    observeEvent(input$update_layer, {
        event.data <-
            event_data('plotly_selected', source = 'plotly_histology')
        if (!is.null(event.data)) {
            isolate({
                ## Now update with the layer input
                rv$layer[sce$key %in% event.data$key] <-
                    input$label_layer
            })
        }
    })

    output$click <- renderPrint({
        event.data <-
            event_data("plotly_click", source = 'plotly_histology')
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a layer guess appear here (double-click to clear)"
            )
        } else {
            isolate({
                ## Now update with the layer input
                if (input$label_click)
                    rv$layer[sce$key %in% event.data$key] <-
                        input$label_layer
            })
            return(event.data$key)
        }
    })

    observeEvent(input$update_layer_gene, {
        event.data <- event_data('plotly_selected', source = 'plotly_gene')
        if (!is.null(event.data)) {
            ## Prepare the data
            sce_sub <- sce[, sce$key %in% event.data$key]
            d <- as.data.frame(colData(sce_sub))
            if (input$geneid %in% colnames(colData(sce_sub))) {
                d$COUNT <- colData(sce_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(sce_sub)[[input$assayname]][which(rowData(sce_sub)$gene_search == input$geneid), ]
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the layer input
                rv$layer[sce$key %in% d$key[!is.na(d$COUNT)]] <-
                    input$label_layer_gene
            })
        }
    })

    output$click_gene <- renderPrint({
        event.data <- event_data("plotly_click", source = 'plotly_gene')
        if (is.null(event.data)) {
            return(
                "Single points clicked and updated with a layer guess appear here (double-click to clear)"
            )
        } else {
            ## Prepare the data
            sce_sub <- sce[, sce$key %in% event.data$key]
            d <- as.data.frame(colData(sce_sub))
            if (input$geneid %in% colnames(colData(sce_sub))) {
                d$COUNT <- colData(sce_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(sce_sub)[[input$assayname]][which(rowData(sce_sub)$gene_search == input$geneid), ]
            }
            d$COUNT[d$COUNT <= input$minCount] <- NA

            isolate({
                ## Now update with the layer input
                if (input$label_click_gene)
                    rv$layer[sce$key %in% d$key[!is.na(d$COUNT)]] <-
                        input$label_layer_gene
            })
            return(event.data$key)
        }
    })

    ## Raw summary
    output$raw_summary <- renderPrint(print(sce),
        width = 80)

    ## Download results
    output$downloadData <- downloadHandler(
        filename = function() {
            gsub(' ',
                '_',
                paste0('spatialLIBD_layerGuesses_', Sys.time(), '.csv'))
        },
        content = function(file) {
            current <- data.frame(
                sample_name = sce$sample_name,
                spot_name = colnames(sce),
                layer = rv$layer,
                stringsAsFactors = FALSE
            )
            ## Keep the NAs?
            if (input$dropNA)
                current <- subset(current, layer != 'NA')
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
            previous_work <- subset(previous_work, layer != 'NA')
            previous_work$key <-
                paste0(previous_work$sample_name,
                    '_',
                    previous_work$spot_name)
            m <- match(previous_work$key, sce$key)
            rv$layer[m[!is.na(m)]] <- previous_work$layer[!is.na(m)]
        }
    })

    ## Reproducibility info
    output$session_info <-
        renderPrint(sessioninfo::session_info(), width = 120)
}
