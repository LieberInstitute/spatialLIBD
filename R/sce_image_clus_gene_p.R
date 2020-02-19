#' Title
#'
#' @param sce
#' @param d
#' @param sampleid
#' @param spatial
#' @param title
#' @param assayname
#' @param viridis
#'
#' @return
#' @export
#'
#' @examples
#'

sce_image_clus_gene_p <-
    function(sce,
        d,
        sampleid,
        spatial,
        title,
        assayname,
        viridis = TRUE) {
        p <-
            ggplot(d,
                aes(
                    x = imagecol,
                    y = imagerow,
                    fill = COUNT,
                    color = COUNT,
                    key =  key
                ))

        if (spatial) {
            p <-
                p + geom_spatial(
                    data = subset(metadata(sce)$image, sample == sampleid),
                    aes(grob = grob),
                    x = 0.5,
                    y = 0.5
                )
        }

        ## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_marker_genes_plots.R
        # add.alpha('black', 0.175)
        # black
        # "#0000002D"

        p <- p +
            geom_point(shape = 21,
                size = 1.25,
                stroke = 0.25) +
            coord_cartesian(expand = FALSE)

        if (viridis) {
            p <- p + scale_fill_gradientn(colors = viridis(21),
                    na.value = c('black' = '#0000002D')) +
                scale_color_gradientn(colors = viridis(21),
                    na.value = c('black' = '#0000002D'))
        } else {
            p <- p +  scale_fill_gradientn(
                colors = c('aquamarine4', 'springgreen', 'goldenrod', 'red'),
                na.value = c('black' = '#0000002D')
            ) + scale_color_gradientn(
                colors = c('aquamarine4', 'springgreen', 'goldenrod', 'red'),
                na.value = c('black' = '#0000002D')
            )
        }

        p <- p +
            xlim(0, max(sce$width)) +
            ylim(max(sce$height), 0) +
            xlab("") + ylab("") +
            labs(fill = NULL, color = NULL) +
            ggtitle(title) +
            theme_set(theme_bw(base_size = 20)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                legend.position = c(0.95, 0.10)
            )
        return(p)
    }
