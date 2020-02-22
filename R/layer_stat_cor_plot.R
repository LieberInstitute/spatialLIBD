#' Visualize the layer modeling correlation of statistics
#'
#' This function makes a heatmap from the [layer_stat_cor()] correlation matrix
#' between a given set of cell cluster/type statistics derived from scRNA-seq
#' or snRNA-seq data (among other types) and the layer statistics from the
#' Human DLPFC Visium data (when using the default arguments).
#'
#' @param cor_stats_layer The output of [layer_stat_cor()].
#' @param max A `numeric(1)` specifying the highest correlation value for the
#' color scale (should be between 0 and 1).
#' @param min A `numeric(1)` specifying the lowest correlation value for the
#' color scale (should be between 0 and -1).
#'
#' @return A heatmap for the correlation matrix between statistics.
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @family Layer correlation functions
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lattice levelplot
#' @details Check
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
#' for a full analysis from which this family of functions is derived from.
#'
#' @examples
#'
#' ## Obtain the necessary data
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' ## Compute the correlations
#' cor_stats_layer <- layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     ori_modeling_results,
#'     'specificity'
#' )
#'
#' ## Visualize the correlation matrix
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
