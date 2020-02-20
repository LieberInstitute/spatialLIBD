#' Title
#'
#' @param cor_stats_layer
#' @param max
#' @param min
#'
#' @return
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lattice levelplot
#'
#' @examples
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' cor_stats_layer <- layer_stat_cor(tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer, ori_modeling_results, 'specificity')
#'
#' layer_stat_cor_plot(cor_stats_layer)
#'
layer_stat_cor_plot <- function(cor_stats_layer, max = 0.81, min = -max) {

    ## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
    theSeq <- seq(min, max, by = 0.01)
    my.col <- colorRampPalette(RColorBrewer::brewer.pal(7, "PRGn"))(length(theSeq))

    lattice::levelplot(
        cor_stats_layer,
        aspect = "fill",
        at = theSeq,
        col.regions = my.col,
        ylab = "",
        xlab = "",
        scales = list(x = list(rot = 90, cex = 1.5), y = list(cex = 1.5))
    )
}
