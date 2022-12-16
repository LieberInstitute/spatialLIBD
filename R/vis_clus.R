#' Sample spatial cluster visualization
#'
#' This function visualizes the clusters for one given sample at the spot-level
#' using (by default) the histology information on the background. To visualize
#' gene-level (or any continuous variable) use [vis_gene()].
#'
#' @inheritParams run_app
#' @param sampleid A `character(1)` specifying which sample to plot from
#' `colData(spe)$sample_name`.
#' @param clustervar A `character(1)` with the name of the `colData(spe)`
#' column that has the cluster values.
#' @param colors A vector of colors to use for visualizing the clusters
#' from `clustervar`. If the vector has names, then those should match the
#' values of `clustervar`.
#' @param spatial A `logical(1)` indicating whether to include the histology
#' layer from [geom_spatial()]. If you plan to use
#' [ggplotly()][plotly::ggplotly] then it's best to set this to `FALSE`.
#' @param image_id A `character(1)` with the name of the image ID you want to
#' use in the background.
#' @param alpha A `numeric(1)` in the `[0, 1]` range that specifies the
#' transparency level of the data on the spots.
#' @param point_size A `numeric(1)` specifying the size of the points. Defaults
#' to `1.25`. Some colors look better if you use `2` for instance.
#' @param auto_crop A `logical(1)` indicating whether to automatically crop
#' the image / plotting area, which is useful if the Visium capture area is
#' not centered on the image and if the image is not a square.
#' @param ... Passed to [paste0()][base::paste] for making the title of the
#' plot following the `sampleid`.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @family Spatial cluster visualization functions
#' @export
#' @importFrom SpatialExperiment spatialCoords
#' @details This function subsets `spe` to the given sample and prepares the
#' data and title for [vis_clus_p()].
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Check the colors defined by Lukas M Weber
#'     libd_layer_colors
#'
#'     ## Use the manual color palette by Lukas M Weber
#'     p1 <- vis_clus(
#'         spe = spe,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers"
#'     )
#'     print(p1)
#'
#'     ## Without auto-cropping the image
#'     p2 <- vis_clus(
#'         spe = spe,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         auto_crop = FALSE,
#'         ... = " LIBD Layers"
#'     )
#'     print(p2)
#'
#'     ## Without histology
#'     p3 <- vis_clus(
#'         spe = spe,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers",
#'         spatial = FALSE
#'     )
#'     print(p3)
#' }
vis_clus <- function(spe,
    sampleid,
    clustervar,
    colors = c(
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
    ),
    spatial = TRUE,
    image_id = "lowres",
    alpha = NA,
    point_size = 2,
    auto_crop = TRUE,
    ...) {
    spe_sub <- spe[, spe$sample_id == sampleid]
    d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)

    vis_clus_p(
        spe = spe_sub,
        d = d,
        clustervar = clustervar,
        sampleid = sampleid,
        spatial = spatial,
        title = paste0(sampleid, ...),
        colors = get_colors(colors, d[, clustervar]),
        image_id = image_id,
        alpha = alpha,
        point_size = point_size,
        auto_crop = auto_crop
    )
}
