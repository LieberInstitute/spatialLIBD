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
#' @param ... Passed to [paste0()][base::paste] for making the title of the
#' plot following the `sampleid`.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @family Spatial cluster visualization functions
#' @export
#' @importFrom SpatialExperiment spatialData
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
#'     vis_clus(
#'         spe = spe,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers"
#'     )
#'
#'     ## Without histology
#'     vis_clus(
#'         spe = spe,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers",
#'         spatial = FALSE
#'     )
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
    ...) {
    spe_sub <- spe[, spe$sample_id == sampleid]
    d <- as.data.frame(SpatialExperiment::spatialData(spe_sub, colData = TRUE, spatialCoords = TRUE))

    vis_clus_p(
        spe = spe_sub,
        d = d,
        clustervar = clustervar,
        sampleid = sampleid,
        spatial = spatial,
        title = paste0(sampleid, ...),
        colors = get_colors(colors, d[, clustervar])
    )
}
