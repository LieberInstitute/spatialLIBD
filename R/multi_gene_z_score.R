#' Combine multiple continuous variables by averaging Z scores
#'
#' To summarize multiple features, each is normalized to represent a Z-score.
#' Scores are averaged to return a single vector.
#'
#' @param cont_mat A \code{matrix()} with spots as rows and 2 or more continuous
#' variables as columns.
#'
#' @return A \code{numeric()} vector with one element per spot, summarizing the
#' multiple continuous variables.
#'
#' @author Nicholas J. Eagles
#' @import MatrixGenerics
#' @family functions for summarizing expression of multiple continuous variables simultaneously
multi_gene_z_score <- function(cont_mat) {
    #   For each spot, average Z-scores across all features
    cont_z <- (cont_mat - rowMeans(cont_mat)) / rowSds(cont_mat)
    z_vec <- colMeans(cont_z, na.rm = TRUE)

    return(z_vec)
}
