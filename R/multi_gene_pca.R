#' Combine multiple continuous variables through PCA
#'
#' PCA is performed on \code{cont_mat}, the matrix of multiple continuous
#' features. The first PC is returned, representing the dominant spatial
#' signature of the feature set. Its direction is negated if necessary so that
#' the majority of coefficients across features are positive (when the features
#' are highly correlated, this encourages spots with higher values to
#' represent areas of higher expression of the features).
#'
#' @param cont_mat A \code{matrix()} with spots as rows and 2 or more continuous
#' variables as columns.
#'
#' @return A \code{numeric()} vector with one element per spot, summarizing the
#' multiple continuous variables.
#'
#' @author Nicholas J. Eagles
#' @importFrom stats prcomp
#' @family functions for summarizing expression of multiple continuous variables simultaneously
#' @keywords internal
multi_gene_pca <- function(cont_mat) {
    #   PCA calculation requires at least 2 features with no NAs and nonzero
    #   variance. Verify this and drop any bad features
    good_indices <- which(
        (colSums(is.na(cont_mat)) == 0) &
            (colSds(cont_mat) != 0)
    )
    if (length(good_indices) < 2) {
        stop("After dropping features with NAs or no expression variation, less than 2 features were left. This error can occur when using data from only 1 spot.", call. = FALSE)
    }
    if (ncol(cont_mat) - length(good_indices) > 0) {
        warning(
            sprintf(
                "Dropping features(s) '%s' which have NAs or no expression variation",
                paste(colnames(cont_mat)[-good_indices], collapse = "', '")
            ),
            call. = FALSE
        )
    }
    cont_mat <- cont_mat[, good_indices]

    pc_exp <- stats::prcomp(cont_mat, center = TRUE, scale = TRUE)
    pc_vec <- pc_exp$x[, "PC1"]

    #   Reverse the direction of PC1 if needed to improve visual interpretation
    #
    #   Often, this function will be called with multiple genes as continuous
    #   variables, and in particular for genes with similar spatial patterns of
    #   expression. In this case, it's likely that each gene's coefficients to
    #   the first PC should tend to have the same sign. Next, the sign of each
    #   PC is arbitary, and we'd like plots to have positive values where
    #   expression is greater. If most genes have negative coefficients to the
    #   first PC, we reverse the sign of the coefficients to make visual
    #   intrepretation consistent. Note this step is neither beneficial nor
    #   harmful in other cases, where continuous features are not expected to be
    #   positively correlated
    if (mean(pc_exp$rotation[, 1] > 0) < 0.5) {
        pc_vec <- -1 * pc_vec
    }

    return(pc_vec)
}
