## Increase the memory size to 50 MB up from the 5 MB default
options(shiny.maxRequestSize = 50 * 1024^2)

shinyServer(function(input, output, session) {

    ## Global variables needed throughout the app
    rv <- reactiveValues(login = FALSE, layer = rep('NA', ncol(sce)))

    ## Authentication
    accessToken <- shiny::callModule(googleSignIn, "gauth_login")
    userDetails <- reactive({
        validate(
            need(accessToken(), "Please authenticate!")
        )
        if(accessToken()$email %in% c(
            'fellgernon@gmail.com',
            'andrewejaffe@gmail.com',
            'kristen.r.maynard@gmail.com',
            'keri.martinowich@gmail.com'
        )) {
            rv$login <- TRUE
            return(TRUE)
        } else {
            stop('Your email is not part of the authorized emails for sceLIBD.')
        }
        
    })

    ## Display user's Google display name after successful login
    output$display_username <- renderText({
        validate(
            need(userDetails(), "Getting user details")
        )
        paste0('You are logged in as: ', accessToken()$name, ' with email ', accessToken()$email)
    })

    ## Render UI after login
    output$sceUI <- renderUI({
        if(rv$login) {
            tagList(
                sidebarLayout(
                    sidebarPanel(
                        selectInput(inputId = 'sample', label = 'Sample to plot', choices = unique(sce$sample_name)),
                        selectInput(inputId = 'cluster', label = 'Clusters to plot', choices = c('Cluster10X', 'Layer', 'Maynard', 'Martinowich', colnames(clust_k5))),
                        pickerInput(inputId = 'geneid', label = 'Gene', choices = sort(genes), selected = genes[17856], options = pickerOptions(liveSearch = TRUE)),
                        selectInput(inputId = 'assayname', label = 'Gene scale', choices = c('counts', 'logcounts'), selected = 'logcounts'),
                        numericInput(inputId = 'minExpr', label = 'Minimum expression value', value = 0, min = -1, max = max(assays(sce)$logcounts), step = 1),
                        hr(),
                        checkboxInput('dropNA', 'Drop NA layer entries in the CSV file?', value = TRUE),
                        downloadButton('downloadData', 'Download layer guesses'),
                        helpText('Save your layer guesses frequently to avoid losing your work!'),
                        hr(),
                        fileInput('priorGuesses', 'Overwrite "Layer" with your prior guesses. You can combine multiple files and re-download the merged results, though note that the order matters though as results are overwritten sequentially!.',
                            accept = c('text/csv', '.csv',
                            'text/comma-separated-values,text/plain')
                        ),
                        helpText('This is useful for resuming your work. It should be a CSV file with the sample_name, spot_name, and layer columns.'),
                        hr(),
                        width = 2
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel('Raw summary',
                                verbatimTextOutput('raw_summary')
                            ),
                            tabPanel('Clusters (static)',
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
                            tabPanel('Clusters (interactive)',
                                plotlyOutput('histology_plotly', width = '600px', height = '600px'),
                                plotlyOutput('histology_plotly_gene'),
                                textInput('label_layer', 'Layer label', 'Your Guess'),
                                checkboxInput('label_click', 'Enable layer-labelling by clicking on points', value = FALSE),
                                verbatimTextOutput("click"),
                                actionButton('update_layer', 'Label selected points (from lasso) with layer'),
                                helpText('Select points (lasso) to label them with a layer guess.'),
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
                            tabPanel('Clusters grid (static)',
                                checkboxGroupInput('grid_samples', label = 'Select samples to show in the grid', choices = unique(sce$sample_name), selected = unique(sce$sample_name)[seq(1, 11, by = 4)], inline = TRUE),
                                numericInput('grid_nrow', label = 'N rows', value = 1, min = 1, max = 3),
                                numericInput('grid_ncol', label = 'N columns', value = 3, min = 1, max = 4),
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
                            tabPanel('Clusters grid (interactive)',
                                checkboxGroupInput('grid_samples_plotly', label = 'Select samples to show in the grid', choices = unique(sce$sample_name), selected = unique(sce$sample_name)[seq(1, 11, by = 4)], inline = TRUE),
                                numericInput('grid_nrow_plotly', label = 'N rows', value = 1, min = 1, max = 3),
                                numericInput('grid_ncol_plotly', label = 'N columns', value = 3, min = 1, max = 4),
                                actionButton('grid_update_plotly', label = 'Update grid plot'),
                                uiOutput('grid_plotly'),
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
                            tabPanel('Gene (static)',
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
                            tabPanel('Gene (interactive)',
                                uiOutput('gene_plotly_cluster_subset_ui'),
                                plotlyOutput('gene_plotly', width = '600px', height = '600px'),
                                plotlyOutput('gene_plotly_clusters'),
                                textInput('label_layer_gene', 'Layer label', 'Your Guess'),
                                checkboxInput('label_click_gene', 'Enable layer-labelling by clicking on points', value = FALSE),
                                verbatimTextOutput("click_gene"),
                                actionButton('update_layer_gene', 'Label selected points (from lasso) with layer'),
                                helpText('Select points to label them with a layer guess.'),
                                helpText('Note that only spots passing the minimum expression value will be updated.'),
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
                            tabPanel('Gene grid (static)',
                                checkboxGroupInput('gene_grid_samples', label = 'Select samples to show in the grid', choices = unique(sce$sample_name), selected = unique(sce$sample_name)[seq(1, 11, by = 4)], inline = TRUE),
                                numericInput('gene_grid_nrow', label = 'N rows', value = 1, min = 1, max = 3),
                                numericInput('gene_grid_ncol', label = 'N columns', value = 3, min = 1, max = 4),
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
                            tabPanel('Documentation',
                                p('There can be a maximum of 36 unique guessed layers before we run out of colors.'),
                                p('todo')
                            )
                        )
                    )
                )
            )
        } else {
            HTML('You have to authenticate to see this section and your email has to have been previously approved.')
        }
    })

    output$helpUI <- renderUI({
        if(rv$login) {
            tagList(
                p('Please get in touch with Leonardo Collado Torres.'),
                hr(),
                p('The following information will be useful to them:'),
                verbatimTextOutput('session_info')
            )
        } else {
            HTML('You have to authenticate to see this section and your email has to have been previously approved.')
        }
    })
    
    ## Set the max based on the assay
    observeEvent(input$assayname, {
        updateNumericInput(session, inputId = 'minExpr', value = 0, min = -1, max = max(assays(sce)[[input$assayname]]), step = 1)
    })
    
    
    ## Clusters/Layers
    output$histology <- renderPlot({
        colors <- NULL
        if(input$cluster %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if(input$cluster == 'Layer') {
            sce$Layer <- rv$layer
            colors <- Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }
        
        sce_image_clus(sce, sampleid = input$sample, clustervar = input$cluster, colors = colors, ... = paste(' with', input$cluster))
    }, width = 600, height = 600)
    
        
    output$grid_static <- renderUI({
        input$grid_update
        
        plotOutput('histology_grid', width = 600 * isolate(input$grid_ncol), height = 600 * isolate(input$grid_nrow))
    })
    
    output$histology_grid <- renderPlot({
        input$grid_update
        
        colors <- NULL
        if(isolate(input$cluster) %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if(isolate(input$cluster == 'Layer')) {
            sce$Layer <- rv$layer
            colors <- Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }
        sce_sub <- sce[, sce$sample_name %in% isolate(input$grid_samples)]
        plots <- sce_image_grid(sce_sub, colData(sce_sub)[[isolate(input$cluster)]], sort_clust = FALSE, colors = colors, return_plots = TRUE, ... = paste(' with', isolate(input$cluster)))
        print(cowplot::plot_grid(plotlist = plots, nrow = isolate(input$grid_nrow), ncol = isolate(input$grid_ncol)))
    }, width = 'auto', height = 'auto')
    
    
    output$gene <- renderPlot({        
        sce_image_clus_gene(sce, sampleid = input$sample, geneid = which(genes == input$geneid), assayname = input$assayname, minExpr = input$minExpr)
    }, width = 600, height = 600)
    
    
    output$gene_grid_static <- renderUI({
        input$gene_grid_update
        
        plotOutput('gene_grid', width = 600 * isolate(input$gene_grid_ncol), height = 600 * isolate(input$gene_grid_nrow))
    })
    
    output$gene_grid <- renderPlot({
        input$gene_grid_update
        
        sce_sub <- sce[, sce$sample_name %in% isolate(input$gene_grid_samples)]
        plots <- sce_image_grid_gene(sce_sub, geneid = isolate(which(genes == input$geneid)), assayname = isolate(input$assayname), minExpr = isolate(input$minExpr), return_plots = TRUE)
        print(cowplot::plot_grid(plotlist = plots, nrow = isolate(input$gene_grid_nrow), ncol = isolate(input$gene_grid_ncol)))
    }, width = 'auto', height = 'auto')
    
   
    ## Plotly versions
    output$histology_plotly <- renderPlotly({
        colors <- NULL
        if(input$cluster %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if(input$cluster == 'Layer') {
            sce$Layer <- rv$layer
            colors <- Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }
        pen <- png::readPNG(file.path('data', input$sample, 'tissue_lowres_image.png'))
        p <- sce_image_clus(sce, sampleid = input$sample, clustervar = input$cluster, spatial = FALSE, colors = colors, ... = paste(' with', input$cluster))
        layout(
            ggplotly(p, width = 600, height = 600, source = 'plotly_histology'),
            images = list(list(
            source = raster2uri(as.raster(pen)),
            xref = "paper", 
            yref = "paper", 
            x = 0, y = 0, 
            sizex = 1, sizey = 1, 
            xanchor = "left", yanchor = "bottom",
            opacity = 1, layer = 'below',
            sizing = 'stretch'
        )), dragmode = 'select')
    })
    
    output$histology_plotly_gene <- renderPlotly({
        event.data <- event_data('plotly_selected', source = 'plotly_histology')
        if(is.null(event.data)) return(NULL)
            
        ## Find which points were selected
        sce_sub <- sce[, sce$key %in% event.data$key]
        
        d <- as.data.frame(colData(sce_sub))
        d$UMI <- assays(sce_sub)[[input$assayname]][which(genes == input$geneid), ]
        d$UMI[d$UMI <= input$minExpr] <- NA
        p <- ggplot(d, aes(x = UMI)) + geom_density() + ggtitle(rowData(sce_sub)$gene_name[which(genes == input$geneid)]) + xlab(input$assayname)
        ggplotly(p)
    })
    
    
    output$grid_plotly <- renderUI({
        input$grid_update_plotly
        
        plotlyOutput('histology_grid_plotly', width = 600 * isolate(input$grid_ncol_plotly), height = 600 * isolate(input$grid_nrow_plotly))
    })
    
    output$histology_grid_plotly <- renderPlotly({
        input$grid_update_plotly
        
        colors <- NULL
        if(isolate(input$cluster) %in% c('Maynard', 'Martinowich')) {
            colors <- cols_layers_martinowich
        }
        if(isolate(input$cluster == 'Layer')) {
            sce$Layer <- rv$layer
            colors <- Polychrome::palette36.colors(length(unique(rv$layer)))
            names(colors) <- unique(rv$layer)
        }
        sce_sub <- sce[, sce$sample_name %in% isolate(input$grid_samples_plotly)]
        plots <- sce_image_grid(sce_sub, colData(sce_sub)[[isolate(input$cluster)]], sort_clust = FALSE, colors = colors, return_plots = TRUE, spatial = FALSE, ... = paste(' with', isolate(input$cluster)))
        
        plots2 <- mapply(function(p, samplename) {
            pen <- png::readPNG(file.path('data', samplename, 'tissue_lowres_image.png'))
            layout(
                ggplotly(p, width = 600, height = 600, source = 'plotly_histology_grid'),
                images = list(list(
                source = raster2uri(as.raster(pen)),
                xref = "paper", 
                yref = "paper", 
                x = 0, y = 0, 
                sizex = 1, sizey = 1, 
                xanchor = "left", yanchor = "bottom",
                opacity = 1, layer = 'below',
                sizing = 'stretch'
            )), dragmode = 'select')
        }, plots, names(plots), SIMPLIFY = FALSE)
        layout(subplot(plots2, nrows = isolate(input$grid_nrow_plotly)), width = 600 * isolate(input$grid_ncol_plotly), height = 600 * isolate(input$grid_nrow_plotly))
        
    })
    
    ## Set the cluster subset options
    output$gene_plotly_cluster_subset_ui <- renderUI({
        input$clusters
        
        if(input$cluster == 'Layer') {
            cluster_opts <- unique(rv$layer)
        } else {
            cluster_opts <- unique(colData(sce)[[input$cluster]])
        }
        checkboxGroupInput('gene_plotly_cluster_subset', label = 'Select clusters to show in this plot', choices = sort(cluster_opts), selected = cluster_opts, inline = TRUE)
    })
    
    output$gene_plotly <- renderPlotly({
        if(is.null(input$gene_plotly_cluster_subset)) return(NULL)
        
        pen <- png::readPNG(file.path('data', input$sample, 'tissue_lowres_image.png'))
        if(input$cluster == 'Layer') {
            cluster_opts <- rv$layer %in% input$gene_plotly_cluster_subset
        } else {
            cluster_opts <- as.character(colData(sce)[[input$cluster]]) %in% input$gene_plotly_cluster_subset
        }
        
        p <- sce_image_clus_gene(sce[, cluster_opts], sampleid = input$sample, geneid = which(genes == input$geneid), assayname = input$assayname, minExpr = input$minExpr, spatial = FALSE)
        layout(
            ggplotly(p, width = 600, height = 600, source = 'plotly_gene'),
            images = list(list(
            source = raster2uri(as.raster(pen)),
            xref = "paper", 
            yref = "paper", 
            x = 0, y = 0, 
            sizex = 1, sizey = 1, 
            xanchor = "left", yanchor = "bottom",
            opacity = 1, layer = 'below',
            sizing = 'stretch'
        )), dragmode = 'select')
    })
    
    
    output$gene_plotly_clusters <- renderPlotly({
        event.data <- event_data('plotly_selected', source = 'plotly_gene')
        if(is.null(event.data)) return(NULL)
            
        ## Prepare the data
        sce$Layer <- rv$layer
        sce_sub <- sce[, sce$key %in% event.data$key]
        d <- as.data.frame(colData(sce_sub))
        d$UMI <- assays(sce_sub)[[input$assayname]][which(genes == input$geneid), ]
        d$UMI[d$UMI <= input$minExpr] <- NA
                
        ## Plot the cluster frequency
        p <- ggplot(subset(d, !is.na(UMI)), aes(x = !!sym(input$cluster))) + geom_bar() + ggtitle(input$cluster)
        ggplotly(p)
    })
    
    
    
    
    
    
    observeEvent(input$update_layer, {
        event.data <- event_data('plotly_selected', source = 'plotly_histology')
        if (!is.null(event.data)) {
            isolate({
                ## Now update with the layer input
                rv$layer[sce$key %in% event.data$key] <- input$label_layer
            })
        }
    })
    
    output$click <- renderPrint({
        event.data <- event_data("plotly_click", source = 'plotly_histology')
        if (is.null(event.data)) {
            return("Single points clicked and updated with a layer guess appear here (double-click to clear)" )
        } else {
            isolate({
                ## Now update with the layer input
                if(input$label_click) rv$layer[sce$key %in% event.data$key] <- input$label_layer
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
            d$UMI <- assays(sce_sub)[[input$assayname]][which(genes == input$geneid), ]
            d$UMI[d$UMI <= input$minExpr] <- NA
        
            isolate({
                ## Now update with the layer input
                rv$layer[sce$key %in% d$key[!is.na(d$UMI)]] <- input$label_layer_gene
            })
        }   
    })
    
    output$click_gene <- renderPrint({
        event.data <- event_data("plotly_click", source = 'plotly_gene')
        if (is.null(event.data)) {
            return("Single points clicked and updated with a layer guess appear here (double-click to clear)" )
        } else {
            ## Prepare the data
            sce_sub <- sce[, sce$key %in% event.data$key]
            d <- as.data.frame(colData(sce_sub))
            d$UMI <- assays(sce_sub)[[input$assayname]][which(genes == input$geneid), ]
            d$UMI[d$UMI <= input$minExpr] <- NA
        
            isolate({
                ## Now update with the layer input
                if(input$label_click_gene) rv$layer[sce$key %in% d$key[!is.na(d$UMI)]] <- input$label_layer_gene
            })
            return(event.data$key)
        }
    })

    ## Raw summary
    output$raw_summary <- renderPrint(
        print(sce),
        width = 80
    )
    
    ## Download results
    output$downloadData <- downloadHandler(
        filename = function() { paste0('sceLIBD_layerGuesses_', Sys.time(), '.csv') },
        content = function(file) {
            current <- data.frame(
                sample_name = sce$sample_name,
                spot_name = colnames(sce),
                layer = rv$layer,
                stringsAsFactors = FALSE
            )
            ## Keep the NAs?
            if(input$dropNA) current <- subset(current, layer != 'NA')
            write.csv(current, file, row.names = FALSE)
        }
    )
    
    ## Upload prior results
    observeEvent(input$priorGuesses, {
        if(!is.null(input$priorGuesses)) {
            previous_work <- read.csv(input$priorGuesses$datapath, header = TRUE,
                stringsAsFactors = FALSE, na.strings = "")
            ## Update the non-NA
            previous_work <- subset(previous_work, layer != 'NA')
            previous_work$key <- paste0(previous_work$sample_name, '_', previous_work$spot_name)
            m <- match(previous_work$key, sce$key)
            rv$layer[m[!is.na(m)]] <- previous_work$layer[!is.na(m)]
        }
    })

    ## Reproducibility info
    output$session_info <- renderPrint(sessioninfo::session_info(), width = 120)
})
