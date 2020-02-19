#' Title
#'
#' @param sce
#' @param d
#' @param sampleid
#' @param spatial
#' @param title
#'
#' @return
#' @export
#'
#' @examples

sce_image_clus_gene_p <-
    function(sce, d, sampleid, spatial, title, assayname) {
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
        p <- p +
            geom_point(shape = 21,
                size = 1.25,
                stroke = 0.25) +
            coord_cartesian(expand = FALSE) +
            scale_fill_gradientn(
                colors = viridis(21)
            ) +
            scale_color_gradientn(
                colors = viridis(21)
            ) +
            xlim(0, max(sce$width)) +
            ylim(max(sce$height), 0) +
            xlab("") + ylab("") +
            labs(fill = assayname, color = assayname) +
            ggtitle(title) +
            theme_set(theme_bw(base_size = 10)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )
        return(p)
    }
