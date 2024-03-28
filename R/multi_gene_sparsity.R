#' Combine multiple continuous variables by proportion of positive values
#'
#' To summarize multiple features, the proportion of features with positive
#' values for each spot is computed.
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
#' @keywords internal
multi_gene_sparsity <- function(cont_mat) {
    return(rowMeans(cont_mat > 0, na.rm = TRUE))
}
