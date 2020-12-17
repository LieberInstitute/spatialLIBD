#' Sample spatial cluster visualization
#'
#' This function visualizes the clusters for one given sample at the spot-level
#' using (by default) the histology information on the background. To visualize
#' gene-level (or any continuous variable) use [sce_image_gene()].
#'
#' @inheritParams run_app
#' @param sampleid A `character(1)` specifying which sample to plot from
#' `colData(sce)$sample_name`.
#' @param clustervar A `character(1)` with the name of the `colData(sce)`
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
#' @details This function subsets `sce` to the given sample and prepares the
#' data and title for [sce_image_clus_p()].
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("sce")) sce <- fetch_data("sce")
#'
#'     ## Check the colors defined by Lukas M Weber
#'     libd_layer_colors
#'
#'     ## Use the manual color palette by Lukas M Weber
#'     sce_image_clus(
#'         sce = sce,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers"
#'     )
#'
#'     ## Works also with VisiumExperiment objects.
#'     sce_image_clus(
#'         sce = sce_to_ve(sce),
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers"
#'     )
#'
#'
#'
#'     ## Without histology
#'     sce_image_clus(
#'         sce = sce,
#'         clustervar = "layer_guess_reordered",
#'         sampleid = "151673",
#'         colors = libd_layer_colors,
#'         ... = " LIBD Layers",
#'         spatial = FALSE
#'     )
#' }
sce_image_clus <- function(sce,
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
    if (is(sce, "VisiumExperiment")) {
        sce_sub <- sce[, SpatialExperiment::spatialCoords(sce)$sample_name == sampleid]
    } else {
        sce_sub <- sce[, sce$sample_name == sampleid]
    }

    d <- as.data.frame(colData(sce_sub))
    sce_image_clus_p(
        sce = sce_sub,
        d = d,
        clustervar = clustervar,
        sampleid = sampleid,
        spatial = spatial,
        title = paste0(sampleid, ...),
        colors = get_colors(colors, d[, clustervar])
    )
}
