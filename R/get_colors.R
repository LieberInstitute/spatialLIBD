#' Title
#'
#' @param colors
#' @param clusters
#'
#' @return
#' @export
#'
#' @examples
#'
#' ori_sce <- fetch_data('sce')
#'
#' cols <- c(Polychrome::palette36.colors(7), 'transparent')
#' names(cols) <- c(levels(ori_sce$layer_guess), 'NA')
#'
#' get_colors(cols, ori_sce$layer_guess)
#'

get_colors <- function(colors = NULL, clusters) {
    n_clus <- length(unique(clusters))

    if (is.null(colors) | n_clus > length(colors)) {
        ## Original ones
        # colors <- c("#b2df8a","#e41a1c","#377eb8","#4daf4a","#ff7f00","gold",
        # "#a65628", "#999999", "black", "grey", "white", "purple")

        ## From https://medialab.github.io/iwanthue/
        ## which I found the link to from
        ## https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
        ## Used the colorblind friendly and default palette

        ## From https://developer.r-project.org/Blog/public/2019/11/21/a-new-palette-for-r/index.html
        ## they point to https://cran.r-project.org/web/packages/Polychrome/vignettes/polychrome.html


        colors <-
            if (n_clus > 12)
                Polychrome::palette36.colors(n_clus)
        else
            c(
                "#b2df8a",
                "#e41a1c",
                "#377eb8",
                "#4daf4a",
                "#ff7f00",
                "gold",
                "#a65628",
                "#999999",
                "black",
                "grey",
                "white",
                "purple"
            )
        names(colors) <- seq_len(length(colors))

    }
    return(colors)
}
