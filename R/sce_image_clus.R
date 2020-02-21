#' Title
#'
#' @param sce
#' @param sampleid
#' @param clustervar
#' @param colors
#' @param spatial
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' ori_sce <- fetch_data('sce')
#'
#' ## Use the manual color palette by Lukas M Weber
#' sce_image_clus(
#'     sce = ori_sce,
#'     clustervar = 'layer_guess_reordered',
#'     sampleid = '151673',
#'     colors = libd_layer_colors,
#'     ... = ' LIBD Layers'
#' )
#'

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
    sce_sub <- sce[, sce$sample_name == sampleid]
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
