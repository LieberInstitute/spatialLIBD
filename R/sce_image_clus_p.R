#' Title
#'
#' @param sce
#' @param d
#' @param clustervar
#' @param sampleid
#' @param colors
#' @param spatial
#' @param title
#'
#' @return
#' @export
#'
#' @examples

sce_image_clus_p <-
    function(sce,
        d,
        clustervar,
        sampleid,
        colors,
        spatial,
        title) {
        if (clustervar %in% c('layer_guess',
            'layer_guess_reordered',
            'layer_guess_reordered_short',
            'spatialLIBD')) {
            title <- gsub(clustervar, 'LIBD Layers', title)
        }

        p <- ggplot(d,
            aes(
                x = imagecol,
                y = imagerow,
                fill = factor(!!sym(clustervar)),
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
            scale_fill_manual(values = colors) +
            xlim(0, max(sce$width)) +
            ylim(max(sce$height), 0) +
            xlab("") + ylab("") +
            labs(fill = NULL) +
            guides(fill = guide_legend(override.aes = list(size = 3))) +
            ggtitle(title) +
            theme_set(theme_bw(base_size = 20)) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                axis.ticks = element_blank()
            )
        return(p)
    }
