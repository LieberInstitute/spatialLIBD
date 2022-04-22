#' Visualize a matrix of values across human brain layers
#'
#' This function visualizes a numerical matrix where the Y-axis represents
#' the human brain layers and can be adjusted to represent the length of
#' each brain layer. Cells can optionally have text values. This function is
#' used by [gene_set_enrichment_plot()] and [layer_stat_cor_plot()].
#'
#' @param matrix_values A `matrix()` with one column per set of interest and
#' one row per layer (group) with numeric values.
#' @param matrix_labels Optionally a character `matrix()` with the same
#' dimensions and `dimnames()` as `matrix_values` with text labels for the
#' cells.
#' @param xlabs A vector of names in the same order and length as
#' `colnames(matrix_values)`.
#' @param layerHeights A `numeric()` vector of length equal to
#' `nrow(matrix_values) + 1` that starts at 0 specifying where
#' to plot the y-axis breaks which can be used for re-creating the length of
#' each brain layer.
#' @param mypal A vector with the color palette to use.
#' @param breaks Passed to [fields::image.plot()]. Used by
#' [layer_stat_cor_plot()].
#' @param axis.args Passed to [fields::image.plot()]. Used by
#' [layer_stat_cor_plot()].
#' @param srt The angle for the x-axis labels. Used by [layer_stat_cor_plot()].
#' @param mar Passed to [graphics::par()].
#' @param cex Used for the x-axis labels and the text inside the cells.
#'
#' @return A base R plot visualizing the input `matrix_values` with optional
#' text labels for `matrix_labels`.
#' @importFrom fields image.plot
#' @importFrom graphics axis text abline par
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#'
#' @examples
#'
#' ## Create some random data
#' set.seed(20200224)
#' mat <- matrix(runif(7 * 8, min = -1), nrow = 7)
#' rownames(mat) <- c("WM", paste0("L", rev(seq_len(6))))
#' colnames(mat) <- paste0("Var", seq_len(8))
#'
#' ## Create some text labels
#' mat_text <- matrix("", nrow = 7, ncol = 8, dimnames = dimnames(mat))
#' diag(mat_text) <- as.character(round(diag(mat), 2))
#'
#' ## Make the plot
#' layer_matrix_plot(mat, mat_text)
#'
#' ## Try to re-create the anatomical proportions of the human brain layers
#' layer_matrix_plot(
#'     mat,
#'     mat_text,
#'     layerHeights = c(0, 40, 55, 75, 85, 110, 120, 135),
#'     cex = 2
#' )
layer_matrix_plot <-
    function(matrix_values,
    matrix_labels = NULL,
    xlabs = NULL,
    layerHeights = NULL,
    mypal = c(
        "white",
        grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(50)
    ),
    breaks = NULL,
    axis.args = NULL,
    srt = 45,
    mar = c(8, 4 + (max(nchar(rownames(matrix_values))) %/% 3) * 0.5, 4, 2) + 0.1,
    cex = 1.2) {
        ## Create some default values in case the user didn't specify them
        if (is.null(xlabs)) {
            if (is.null(colnames(matrix_values))) {
                xlabs <- paste0("V", seq_len(ncol(matrix_values)))
            } else {
                xlabs <- colnames(matrix_values)
            }
        }

        if (is.null(layerHeights)) {
            layerHeights <- c(0, seq_len(nrow(matrix_values))) * 15
        }

        if (is.null(matrix_labels)) {
            ## Make an empty matrix of labels if none were specified
            matrix_labels <-
                matrix(
                    "",
                    ncol = ncol(matrix_values),
                    nrow = nrow(matrix_values),
                    dimnames = dimnames(matrix_values)
                )
        }

        ## Check inputs
        stopifnot(length(layerHeights) == nrow(matrix_values) + 1)
        stopifnot(length(xlabs) == ncol(matrix_values))
        stopifnot(layerHeights[1] == 0)



        ## For the y-axis labels
        midpoint <- function(x) {
            x[-length(x)] + diff(x) / 2
        }

        ## Make the plot
        par(mar = mar)
        fields::image.plot(
            x = seq(0, ncol(matrix_values), by = 1),
            y = layerHeights,
            z = as.matrix(t(matrix_values)),
            col = mypal,
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = "",
            breaks = breaks,
            nlevel = length(mypal),
            axis.args = axis.args
        )
        axis(2,
            rownames(matrix_labels),
            at = midpoint(layerHeights),
            las = 1
        )
        axis(1, rep("", ncol(matrix_values)), at = seq(0.5, ncol(matrix_values) - 0.5))
        text(
            x = seq(0.5, ncol(matrix_values) - 0.5),
            y = -1 * max(nchar(xlabs)) / 2,
            xlabs,
            xpd = TRUE,
            srt = srt,
            cex = cex,
            adj = 1
        )
        abline(h = layerHeights, v = c(0, seq_len(ncol(matrix_values))))
        text(
            x = rep(seq(0.5, ncol(matrix_values) - 0.5), each = nrow(matrix_values)),
            y = rep(midpoint(layerHeights), ncol(matrix_values)),
            as.character(matrix_labels),
            cex = cex * 3 / 4,
            font = 2
        )
    }
