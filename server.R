## Increase the memory size to 50 MB up from the 5 MB default
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

shinyServer(function(input, output, session) {
    ## Global variables needed throughout the app
    rv <-
        reactiveValues(login = FALSE, layer = rep('NA', ncol(sce)))
    
    ## Authentication
    accessToken <- shiny::callModule(googleSignIn, "gauth_login")
    userDetails <- reactive({
        validate(need(accessToken(), "Please authenticate!"))
        if (accessToken()$email %in% c(
            'fellgernon@gmail.com',
            'andrewejaffe@gmail.com',
            'kristen.r.maynard@gmail.com',
            'keri.martinowich@gmail.com',
            'stephaniechicks@gmail.com',
            'lukas.weber.edu@gmail.com',
            'zach.besich@gmail.com'
        )) {
            rv$login <- TRUE
            return(TRUE)
        } else {
            stop('Your email is not part of the authorized emails for spatialLIBD.')
        }
        
    })
    
    ## Display user's Google display name after successful login
    output$display_username <- renderText({
        validate(need(userDetails(), "Getting user details"))
        paste0('You are logged in as: ',
            accessToken()$name,
            ' with email ',
            accessToken()$email)
    })
    
    ## Render UI after login
    output$sceUI <- renderUI({
        if (rv$login) {
            tagList(sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = 'sample',
                        label = 'Sample to plot',
                        choices = unique(sce$sample_name)
                    ),
                    selectInput(
                        inputId = 'cluster',
                        label = 'Clusters to plot',
                        choices = c(
                            'Cluster10X',
                            'Layer',
                            'Maynard',
                            'Martinowich',
                            colnames(clust_k5)
                        )
                    ),
                    selectInput(
                        inputId = 'reduced_name',
                        label = 'Reduced dimensions',
                        choices = reducedDimNames(sce)[-1],
                        selected = reducedDimNames(sce)[length(reducedDimNames(sce))]
                    ),
                    pickerInput(
                        inputId = 'geneid',
                        label = 'Gene (or count variable)',
                        choices = c('cell_count', 'sum_umi', 'sum_gene', sort(genes)),
                        selected = 'cell_count',
                        options = pickerOptions(liveSearch = TRUE)
                    ),
                    selectInput(
                        inputId = 'assayname',
                        label = 'Gene scale',
                        choices = c('counts', 'logcounts'),
                        selected = 'logcounts'
                    ),
                    numericInput(
                        inputId = 'minCount',
                        label = 'Minimum count value',
                        value = 0,
                        min = -1,
                        max = max(assays(sce)$logcounts),
                        step = 1
                    ),
                    hr(),
                    checkboxInput(
                        'dropNA',
                        'Drop NA layer entries in the CSV file?',
                        value = TRUE
                    ),
                    downloadButton('downloadData', 'Download layer guesses'),
                    helpText(
                        'Save your layer guesses frequently to avoid losing your work!'
                    ),
                    hr(),
                    fileInput(
                        'priorGuesses',
                        'Overwrite "Layer" with your prior guesses. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                        accept = c(
                            'text/csv',
                            '.csv',
                            'text/comma-separated-values,text/plain'
                        )
                    ),
                    helpText(
                        'This is useful for resuming your work. It should be a CSV file with the sample_name, spot_name, and layer columns.'
                    ),
                    hr(),
                    width = 2
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel('Raw summary',
                            verbatimTextOutput('raw_summary')),
                        tabPanel(
                            'Clusters (static)',
                            plotOutput('histology'),
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
                            'Clusters (interactive)',
                            plotlyOutput(
                                'histology_plotly',
                                width = '1200px',
                                height = '1200px'
                            ),
                            plotlyOutput('histology_plotly_gene'),
                            textInput('label_layer', 'Layer label', 'Your Guess'),
                            checkboxInput(
                                'label_click',
                                'Enable layer-labelling by clicking on points',
                                value = FALSE
                            ),
                            verbatimTextOutput("click"),
                            actionButton(
                                'update_layer',
                                'Label selected points (from lasso) with layer'
                            ),
                            helpText(
                                'Select points (lasso) to label them with a layer guess.'
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
                            'Clusters grid (static)',
                            checkboxGroupInput(
                                'grid_samples',
                                label = 'Select samples to show in the grid',
                                choices = unique(sce$sample_name),
                                selected = unique(sce$sample_name)[seq(1, 11, by = 4)],
                                inline = TRUE
                            ),
                            numericInput(
                                'grid_nrow',
                                label = 'N rows',
                                value = 1,
                                min = 1,
                                max = 3
                            ),
                            numericInput(
                                'grid_ncol',
                                label = 'N columns',
                                value = 3,
                                min = 1,
                                max = 4
                            ),
                            actionButton('grid_update', label = 'Update grid plot'),
                            uiOutput('grid_static'),
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
                            'Gene (static)',
                            plotOutput('gene'),
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
                            'Gene (interactive)',
                            uiOutput('gene_plotly_cluster_subset_ui'),
                            plotlyOutput(
                                'gene_plotly',
                                width = '600px',
                                height = '600px'
                            ),
                            plotlyOutput('gene_plotly_clusters'),
                            textInput('label_layer_gene', 'Layer label', 'Your Guess'),
                            checkboxInput(
                                'label_click_gene',
                                'Enable layer-labelling by clicking on points',
                                value = FALSE
                            ),
                            verbatimTextOutput("click_gene"),
                            actionButton(
                                'update_layer_gene',
                                'Label selected points (from lasso) with layer'
                            ),
                            helpText('Select points to label them with a layer guess.'),
                            helpText(
                                'Note that only spots passing the minimum count value will be updated.'
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
                            'Gene grid (static)',
                            checkboxGroupInput(
                                'gene_grid_samples',
                                label = 'Select samples to show in the grid',
                                choices = unique(sce$sample_name),
                                selected = unique(sce$sample_name)[seq(1, 11, by = 4)],
                                inline = TRUE
                            ),
                            numericInput(
                                'gene_grid_nrow',
                                label = 'N rows',
                                value = 1,
                                min = 1,
                                max = 3
                            ),
                            numericInput(
                                'gene_grid_ncol',
                                label = 'N columns',
                                value = 3,
                                min = 1,
                                max = 4
                            ),
                            actionButton('gene_grid_update', label = 'Update grid plot'),
                            uiOutput('gene_grid_static'),
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
                            'Documentation',
                            p(
                                'There can be a maximum of 36 unique guessed layers before we run out of colors.'
                            ),
                            p('todo')
                        )
                    )
                )
            ))
        } else {
            HTML(
                'You have to authenticate to see this section and your email has to have been previously approved.'
            )
        }
    })
    
    output$helpUI <- renderUI({
        if (rv$login) {
            tagList(
                p('Please get in touch with Leonardo Collado Torres.'),
                hr(),
                p('The following information will be useful to them:'),
                verbatimTextOutput('session_info')
            )
        } else {
            HTML(
                'You have to authenticate to see this section and your email has to have been previously approved.'
            )
        }
    })
    
    ## Set the max based on the assay
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
    
    
    ## Clusters/Layers
    output$histology <- renderPlot({
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
        print(cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$grid_nrow),
            ncol = isolate(input$grid_ncol)
        ))
    }, width = 'auto', height = 'auto')
    
    
    output$gene <- renderPlot({
        sce_image_clus_gene(
            sce,
            sampleid = input$sample,
            geneid = input$geneid,
            assayname = input$assayname,
            minCount = input$minCount
        )
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
        input$gene_grid_update
        
        sce_sub <-
            sce[, sce$sample_name %in% isolate(input$gene_grid_samples)]
        plots <-
            sce_image_grid_gene(
                sce_sub,
                geneid = isolate(input$geneid),
                assayname = isolate(input$assayname),
                minCount = isolate(input$minCount),
                return_plots = TRUE
            )
        print(cowplot::plot_grid(
            plotlist = plots,
            nrow = isolate(input$gene_grid_nrow),
            ncol = isolate(input$gene_grid_ncol)
        ))
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
        # geneid <- genes[17856]
        # assayname <- 'logcounts'
        # minCount <- 0
        # clustervar <- 'Cluster10X'
        # colors <- get_colors(NULL, sce$Cluster10X)
        # reduced_name <- 'TSNE_perplexity50'
        
        ## Read in the histology image
        pen <-
            png::readPNG(file.path('data', sampleid, 'tissue_lowres_image.png'))
        
        
        ## From sce_image_clus_gene() in global.R
        sce_sub <- sce[, sce$sample_name == sampleid]
        d <- as.data.frame(colData(sce_sub))
        if(geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) {
            d$COUNT <- colData(sce_sub)[[geneid]]
        } else {
            d$COUNT <- assays(sce_sub)[[assayname]][which(genes == geneid),]
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
                if(!geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) assayname,
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
            title = ""
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
                stroke = 0.25) +
            scale_fill_gradientn(colors = viridis(21), guide = FALSE) +
            scale_color_gradientn(colors = viridis(21), guide = FALSE) +
            labs(fill = "COUNT") +
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
        if (input$geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) {
            d$COUNT <- colData(sce_sub)[[input$geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[input$assayname]][which(genes == input$geneid),]
        }
        d$COUNT[d$COUNT <= input$minCount] <- NA
        p <-
            ggplot(d, aes(x = COUNT)) + geom_density() + ggtitle(input$geneid) + xlab(ifelse(
                !input$geneid %in% c('cell_count', 'sum_umi', 'sum_gene'),
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
            png::readPNG(file.path('data', input$sample, 'tissue_lowres_image.png'))
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
                spatial = FALSE
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
        if (input$geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) {
            d$COUNT <- colData(sce_sub)[[input$geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[input$assayname]][which(genes == input$geneid), ]
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
            if (input$geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) {
                d$COUNT <- colData(sce_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(sce_sub)[[input$assayname]][which(genes == input$geneid),]
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
            if (input$geneid %in% c('cell_count', 'sum_umi', 'sum_gene')) {
                d$COUNT <- colData(sce_sub)[[input$geneid]]
            } else {
                d$COUNT <-
                    assays(sce_sub)[[input$assayname]][which(genes == input$geneid),]
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
            paste0('spatialLIBD_layerGuesses_', Sys.time(), '.csv')
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
})
