#' A ggplot2 layer for visualizing the Visium histology
#'
#' This function defines a [ggplot2::layer()] for visualizing the histology
#' image from Visium. It can be combined with other ggplot2 functions for
#' visualizing the clusters as in [vis_clus_p()] or gene-level information
#' as in [vis_gene_p()].
#'
#' @param mapping Passed to `ggplot2::layer(mapping)` where `grob`, `x` and `y`
#' are required.
#' @param data Passed to `ggplot2::layer(data)`.
#' @param stat Passed to `ggplot2::layer(stat)`.
#' @param position Passed to `ggplot2::layer(position)`.
#' @param na.rm Passed to `ggplot2::layer(params = list(na.rm))`.
#' @param show.legend Passed to `ggplot2::layer(show.legend)`.
#' @param inherit.aes Passed to `ggplot2::layer(inherit.aes)`.
#' @param ... Other arguments passed to `ggplot2::layer(params = list(...))`.
#'
#' @return A [ggplot2::layer()] for the histology information.
#' @author 10x Genomics
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Select the first sample and extract the data
#'     sample_id <- unique(spe$sample_id)[1]
#'     spe_sub <- spe[, spe$sample_id == sample_id]
#'     sample_df <- as.data.frame(colData(spe_sub), optional = TRUE)
#'
#'     ## Obtain the histology image
#'     img <- SpatialExperiment::imgRaster(spe_sub)
#'
#'     ## Transform to a rasterGrob object
#'     grob <- grid::rasterGrob(img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"))
#'
#'     ## Make a plot using geom_spatial
#'     p <- ggplot2::ggplot(
#'         sample_df,
#'         ggplot2::aes(
#'             x = pxl_col_in_fullres * SpatialExperiment::scaleFactors(spe_sub),
#'             y = pxl_row_in_fullres * SpatialExperiment::scaleFactors(spe_sub),
#'         )
#'     ) +
#'         geom_spatial(
#'             data = tibble::tibble(grob = list(grob)),
#'             ggplot2::aes(grob = grob),
#'             x = 0.5,
#'             y = 0.5
#'         )
#'
#'     ## Show the plot
#'     print(p)
#'
#'     ## Clean up
#'     rm(spe_sub)
#' }
geom_spatial <- function(
        mapping = NULL,
        data = NULL,
        stat = "identity",
        position = "identity",
        na.rm = FALSE,
        show.legend = NA,
        inherit.aes = FALSE,
        ...) {
    ## To avoid a NOTE on R CMD check
    ggname <- function(prefix, grob) {
        grob$name <- grid::grobName(grob, prefix)
        grob
    }

    GeomCustom <- ggproto(
        "GeomCustom",
        Geom,
        setup_data = function(self, data, params) {
            data <- ggproto_parent(Geom, self)$setup_data(data, params)
            data
        },
        draw_group = function(data, panel_scales, coord) {
            vp <- grid::viewport(x = data$x, y = data$y)
            g <- grid::editGrob(data$grob[[1]], vp = vp)
            ggname("geom_spatial", g)
        },
        required_aes = c("grob", "x", "y")
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
