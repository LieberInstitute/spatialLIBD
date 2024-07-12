#' Obtain the colors for a set of cluster names
#'
#' This function returns a vector of colors based on a vector of cluster
#' names. It can be used to automatically assign colors.
#'
#' @param colors A vector of colors. If `NULL` then a set of default colors will
#' be used when `clusters` has less than 12 unique values, otherwise
#' `Polychrome::palette36` will be used which can
#' generate up to 36 unique colors. If the number of unique clusters is beyond
#' 36 then this function will fail.
#' @param clusters A vector of cluster names.
#'
#' @return A named vector where the values are the colors to use for
#' displaying them different clusters. For some use cases, you might have to
#' either change the names or use [unname()][base::unname].
#'
#' @export
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists("sce_layer")) sce_layer <- fetch_data("sce_layer")
#'
#' ## Example layer colors with the corresponding names
#' get_colors(libd_layer_colors, sce_layer$layer_guess)
#' get_colors(libd_layer_colors, sce_layer$layer_guess_reordered_short)
#'
#' ## Example where colors are assigned automatically
#' ## based on a pre-defined set of colors
#' get_colors(clusters = sce_layer$kmeans_k7)
#'
#' ## Example where Polychrome::palette36.colors() gets used
#' get_colors(clusters = letters[seq_len(13)])
#'
#' ## What happens if you have a logical variable with NAs?
#' set.seed(20240712)
#' log_var <- sample(c(TRUE, FALSE, NA),
#'     1000,
#'     replace = TRUE,
#'     prob = c(0.3, 0.15, 0.55))
#' log_var_sorted <- sort_clusters(log_var)
#' ## A color does get assigned to 'NA', but will be overwritten by
#' ## 'na_color' passed to `vis_clus_p()` and related functions.
#' get_colors(colors = NULL, clusters = log_var_sorted)
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
            if (n_clus > 12) {
                paletteer::paletteer_d("Polychrome::palette36", n_clus)
            } else {
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
            }
        ## Subset to the actual number of values if we are working with < 12
        colors <- colors[seq_len(n_clus)]

        ## Set the names of the colors in a way compatible with how names
        ## are set in vis_clus_p().
        names(colors) <- levels(factor(clusters))
    } else if (all(unique(as.character(clusters)) %in% c(gsub("ayer", "", names(colors)), NA))) {
        names(colors) <- gsub("ayer", "", names(colors))
    }
    return(colors)
}
