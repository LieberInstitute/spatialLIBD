library('shiny')
library('googleAuthR')
library('SingleCellExperiment')
library('ggplot2')
library('cowplot')
library('plotly')
library('rafalib')
library('viridisLite')
library('shinyWidgets')
library('sessioninfo')

# options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/plus.me", "https://www.googleapis.com/auth/userinfo.email"))
options("googleAuthR.webapp.client_id" = "239106802594-k5p2ki25a1dcfe4p3ab15t4ubs7esal4.apps.googleusercontent.com")
# options("googleAuthR.webapp.client_id" = "558595696226-ug6a54vq8i5g7flrka2vm7n8v7003clg.apps.googleusercontent.com")
# options("googleAuthR.webapp.client_secret" = "MiJPJo781v88MJ06YujmMr58")


load('data/Human_DLPFC_Visium_processedData_sce_scran.Rdata')
load('data/clust_k5_list.Rdata')
load('data/clust_10x_layer_maynard_martinowich.Rdata')


clust_k5 <- do.call(cbind, clust_k5_list)
colnames(clust_k5) <- paste0('SNN_k50_', colnames(clust_k5))
rownames(clust_k5) <- NULL
colData(sce) <- cbind(colData(sce), clust_k5)

sce$Maynard <- clust_10x_layer_maynard
sce$Martinowich <- clust_10x_layer_martinowich

sce$Layer <- NA

genes <- paste0(rowData(sce)$gene_name, '; ', rowData(sce)$gene_id)



geom_spatial <-  function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = FALSE,
                         ...) {
  GeomCustom <- ggproto(
    "GeomCustom",
    Geom,
    setup_data = function(self, data, params) {
      data <- ggproto_parent(Geom, self)$setup_data(data, params)
      data
    },
    
    draw_group = function(data, panel_scales, coord) {
      vp <- grid::viewport(x=data$x, y=data$y)
      g <- grid::editGrob(data$grob[[1]], vp=vp)
      ggplot2:::ggname("geom_spatial", g)
    },
    
    required_aes = c("grob","x","y")
    
  )
  
  layer(
    geom = GeomCustom,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


get_colors <- function(colors = NULL, clusters) {
    n_clus <- length(unique(clusters))
    
    if(is.null(colors) | n_clus > length(colors)) {
        ## Original ones
        # colors <- c("#b2df8a","#e41a1c","#377eb8","#4daf4a","#ff7f00","gold",
        # "#a65628", "#999999", "black", "grey", "white", "purple")
        
        ## From https://medialab.github.io/iwanthue/
        ## which I found the link to from
        ## https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
        
        ## Used the colorblind friendly and default palette
        
        ## Color-blind friendly
        # colors <- if(n_clus > 10) c("#573487", "#96b43d", "#5771dd", "#d39830", "#a874d7", "#64c36f", "#c86ebd", "#47bb8a", "#892862", "#33d4d1", "#db5358", "#6a98e4", "#c55e32", "#516bba", "#b3ad52", "#d55f90", "#588234", "#b8465e", "#a97a35", "#a44739") else if (n_clus <= 6) c("#98a441", "#6778d0", "#50b47b","#9750a1", "#ba6437", "#ba496b") else c("#bfa239", "#5a3a8e", "#799f44", "#c771c4", "#54b06c", "#b0457b", "#43c9b0", "#b8434e", "#6e81da", "#b86738")
        ## Default one
        # colors <- if(n_clus > 10) c("#aab539", "#6f66d7", "#59b94d", "#bd54c1", "#598124", "#d1478f", "#58bd91", "#d4465a", "#48bbd2", "#d6542d", "#6288cc", "#d69938", "#8d61ab", "#a3b26a", "#da8dc6", "#407e4a", "#a24b66", "#86712e", "#e39178", "#a75932") else if (n_clus <= 6) c("#b88f40", "#7a75cd", "#6ca74d", "#c45ca2", "#49adaa", "#cb584c") else c("#6ab64c", "#8761cc", "#c1a942", "#688bcc", "#d35238", "#4db598", "#c361aa", "#677e39", "#c9566e", "#c07b44")
        
        ## Default one if n_clus > 12, otherwise original colors (re-ordered a bit)
        #colors <- if(n_clus > 12) c("#aab539", "#6f66d7", "#59b94d", "#bd54c1", "#598124", "#d1478f", "#58bd91", "#d4465a", "#48bbd2", "#d6542d", "#6288cc", "#d69938", "#8d61ab", "#a3b26a", "#da8dc6", "#407e4a", "#a24b66", "#86712e", "#e39178", "#a75932") else c("#377eb8", "gold", "#ff7f00", "#e41a1c", "#4daf4a", "#b2df8a","#a65628", "#999999", "black", "grey", "white", "purple")
        colors <- if(n_clus > 12) c("#de84b0", "#78bb40", "#9c45bd", "#46c06f", "#cb3e97", "#488733", "#b978e9", "#beae36", "#5c6ade", "#db9443", "#5985dc", "#cf4e32", "#43c4c4", "#d84068", "#5fb88e", "#e471d1", "#327e58", "#7454b1", "#a4b266", "#964f95", "#72722a", "#c18cd3", "#a06332","#54a4d6", "#dc8074", "#5465a4", "#9f4765", "#a09cdf") else c("#377eb8", "gold", "#ff7f00", "#e41a1c", "#4daf4a", "#b2df8a","#a65628", "#999999", "black", "grey", "white", "purple")
        names(colors) <- seq_len(length(colors))
        
    }
    return(colors)
}



sce_image_clus <- function(sce, sampleid, clustervar,
    colors = c("#b2df8a","#e41a1c","#377eb8","#4daf4a","#ff7f00","gold",
        "#a65628", "#999999", "black", "grey", "white", "purple"), spatial = TRUE, ...) {
    
    d <- as.data.frame(colData(sce[, sce$sample_name == sampleid]))
    colors <- get_colors(colors, d[, clustervar])
    p <- ggplot(d,
        aes(x = imagecol, y = imagerow, fill = factor(!!sym(clustervar))))
    if(spatial) {
        p <- p + geom_spatial(data = subset(metadata(sce)$image, sample == sampleid), aes(grob = grob), x = 0.5, y = 0.5)
    }
    p <- p +
    	geom_point(shape = 21, size = 1.25, stroke = 0.25)+
    	coord_cartesian(expand = FALSE)+
    	scale_fill_manual(values = colors) +
    	xlim(0, max(sce$width)) +
    	ylim(max(sce$height), 0) +
    	xlab("") + ylab("") +
    	labs(fill = clustervar)+
    	guides(fill = guide_legend(override.aes = list(size=3)))+
    	ggtitle(paste0(sampleid, ...)) +
    	theme_set(theme_bw(base_size = 10))+
    	theme(panel.grid.major = element_blank(), 
    		panel.grid.minor = element_blank(),
    		panel.background = element_blank(), 
    		axis.line = element_line(colour = "black"),
    		axis.text = element_blank(),
    		axis.ticks = element_blank())
    return(p)
}

sce_image_clus_gene <- function(sce, sampleid, geneid = 17856, spatial = TRUE, assayname = 'logcounts', minExpr = 0, ...) {
    sce_sub <- sce[, sce$sample_name == sampleid]
    d <- as.data.frame(colData(sce_sub))
    d$UMI <- assays(sce_sub)[[assayname]][geneid, ]
    d$UMI[d$UMI <= minExpr] <- NA
    p <- ggplot(d, aes(x = imagecol, y = imagerow, fill = UMI, color = UMI))
    if(spatial) {
        p <- p + geom_spatial(data = subset(metadata(sce)$image, sample == sampleid), aes(grob = grob), x = 0.5, y = 0.5)
    }
    p <- p +
    	geom_point(shape = 21,  size = 1.25, stroke = 0.25) +
    	coord_cartesian(expand=FALSE)+
    	scale_fill_gradientn(colours = viridis(100))+
        scale_color_gradientn(colors = viridis(100))+
    	xlim(0, max(sce$width)) +
    	ylim(max(sce$height), 0) +
    	xlab("") + ylab("") +
    	labs(fill = "UMI")+
    	ggtitle(paste(unique(sce_sub$sample_name), rowData(sce_sub)$gene_name[geneid], assayname, paste0('min Expr: >', minExpr), ..., sep=" - ")) +
    	theme_set(theme_bw(base_size = 10))+
    	theme(panel.grid.major = element_blank(), 
    			panel.grid.minor = element_blank(),
    			panel.background = element_blank(), 
    			axis.line = element_line(colour = "black"),
    			axis.text = element_blank(),
    			axis.ticks = element_blank())
    return(p)
}

# sce_image_clus_gene(sce, '151507')
# sce_image_clus_gene(sce, '151507', minExpr = 3)
# sce_image_clus_gene(sce, '151507', minExpr = 3, assayname = 'counts')


sort_clusters <- function(clusters, map_subset = NULL) {
    if(is.null(map_subset)) {
        map_subset <- rep(TRUE, length(clusters))
    }
    map <- rank(length(clusters[map_subset]) - table(clusters[map_subset]), ties.method = 'first')
    res <- map[clusters]
    factor(res)
}

sce_image_grid <- function(sce, clusters, pdf_file, sort_clust = TRUE, colors = NULL, return_plots = FALSE, spatial = TRUE, ...) {
    colors <- get_colors(colors, clusters)
    sce$Clus <- if(sort_clust) sort_clusters(clusters) else clusters
    plots <- lapply(unique(sce$sample_name), function(sampleid) {
        sce_image_clus(sce, sampleid, 'Clus', colors = colors, spatial = spatial, ...)
    })
    names(plots) <- unique(sce$sample_name)
    if(!return_plots) {
        pdf(pdf_file, height = 24, width = 36)
        print(cowplot::plot_grid(plotlist = plots))
        dev.off()
        return(pdf_file)
    }
    else {
        return(plots)
    }
}

sce_image_grid_gene <- function(sce, geneid = 17856, pdf_file, assayname = 'logcounts', minExpr = 0, return_plots = FALSE, spatial = TRUE, ...) {
    plots <- lapply(unique(sce$sample_name), function(sampleid) {
        sce_image_clus_gene(sce, sampleid, geneid, spatial, assayname, minExpr, ...)
    })
    names(plots) <- unique(sce$sample_name)
    if(!return_plots) {
        pdf(pdf_file, height = 24, width = 36)
        print(cowplot::plot_grid(plotlist = plots))
        dev.off()
        return(pdf_file)
    }
    else {
        return(plots)
    }
}

# plots <- sce_image_grid_gene(sce, minExpr = 3, return_plots = TRUE)
# print(cowplot::plot_grid(plotlist = plots))

sce_image_grid_comp <- function(sce, clus1, clus2, pdf_file, map_subset = NULL, colors = NULL, return_plots = FALSE, ...) {
    clus1 <- sort_clusters(clus1, map_subset)
    clus2 <- sort_clusters(clus2, map_subset)
    if(is.null(colors)) {
        colors <- c('FALSE' = 'red', 'TRUE' = 'grey80')
    }
    clus_agree <- factor(clus1 == clus2, levels = c('FALSE', 'TRUE'))
    sce_image_grid(sce, clus_agree, pdf_file, sort_clust = FALSE, colors = colors, return_plots = return_plots, ...)
}


sce_image_grid_by_clus <- function(sce, clusters, pdf_file, colors = NULL, spatial = TRUE, ...) {
    if(is.null(colors)) {
        colors <- c('FALSE' = 'transparent', 'TRUE' = 'red')
    }
    clusters_uni <- sort(unique(clusters))
    pdf(pdf_file, height = 24, width = 36)
    lapply(clusters_uni, function(clus) {
        curr_clus <- factor(clusters == clus, levels = c('FALSE', 'TRUE'))
        plots <- sce_image_grid(sce, curr_clus, sort_clust = FALSE, colors = colors, return_plots = TRUE, spatial = spatial, ... = paste(..., '- cluster', clus))
        print(cowplot::plot_grid(plotlist = plots))
        return(clus)
    })
    dev.off()
    return(pdf_file)
}
