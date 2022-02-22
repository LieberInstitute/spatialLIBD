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
#' @param layerHeights A `numeric()` vector of length equal to
#' `ncol(cor_stats_layer) + 1` that starts at 0 specifying where
#' to plot the y-axis breaks which can be used for re-creating the length of
#' each brain layer. Gets passed to [layer_matrix_plot()].
#' @param cex Passed to [layer_matrix_plot()].
#'
#' @return A heatmap for the correlation matrix between statistics.
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @family Layer correlation functions
#' @seealso layer_matrix_plot
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @details Check
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
#' for a full analysis from which this family of functions is derived from.
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists("modeling_results")) {
#'     modeling_results <- fetch_data(type = "modeling_results")
#' }
#'
#' ## Compute the correlations
#' cor_stats_layer <- layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     modeling_results,
#'     model_type = "enrichment"
#' )
#'
#' ## Visualize the correlation matrix
#' layer_stat_cor_plot(cor_stats_layer, max = max(cor_stats_layer))
#'
#' ## Restrict the range of colors further
#' layer_stat_cor_plot(cor_stats_layer, max = 0.3)
#'
#' ## Repeat with just the top 10 layer marker genes
#' layer_stat_cor_plot(layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     modeling_results,
#'     model_type = "enrichment",
#'     top_n = 10
#' ), max = 0.3)
#'
#' ## Now with the "pairwise" modeling results and also top_n = 10
#' layer_stat_cor_plot(layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     modeling_results,
#'     model_type = "pairwise",
#'     top_n = 10
#' ), max = 0.3)
layer_stat_cor_plot <-
    function(cor_stats_layer,
    max = 0.81,
    min = -max,
    layerHeights = NULL,
    cex = 1.2) {
        ## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
        theSeq <- seq(min, max, by = 0.01)
        my.col <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(7, "PRGn"))(length(theSeq))

        ## Subset values
        cor_stats_layer[cor_stats_layer <= min] <- min
        cor_stats_layer[cor_stats_layer >= max] <- max

        ## Re-shape the matrix
        mat_vals <- t(cor_stats_layer)

        ## Re-order and shorten names if they match our data
        if (all(rownames(mat_vals) %in% c("WM", paste0("Layer", seq_len(6))))) {
            rownames(mat_vals) <- gsub("ayer", "", rownames(mat_vals))
            mat_vals <- mat_vals[c("WM", paste0("L", rev(seq_len(6)))), , drop = FALSE]

            ## Use our default layer heights also
            if (is.null(layerHeights)) {
                layerHeights <- c(0, 40, 55, 75, 85, 110, 120, 135)
            }
        }

        ## From fields:::imagePlotInfo
        midpoints <- seq(min, max, length.out = length(my.col))
        delta <- (midpoints[2] - midpoints[1]) / 2
        breaks <- c(midpoints[1] - delta, midpoints + delta)

        legend_cuts <- seq(-1, 1, by = 0.1)
        legend_cuts <- legend_cuts[legend_cuts >= min & legend_cuts <= max]
        axis.args <- list(
            at = legend_cuts,
            labels = legend_cuts
        )

        layer_matrix_plot(
            matrix_values = mat_vals,
            matrix_labels = NULL,
            xlabs = NULL,
            layerHeights = layerHeights,
            mypal = my.col,
            breaks = breaks,
            axis.args = axis.args,
            srt = 90,
            cex = cex
        )
    }
