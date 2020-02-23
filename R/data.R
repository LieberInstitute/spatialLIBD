#' Vector of LIBD layer colors
#'
#' A named vector of colors to use for the LIBD layers designed by Lukas M.
#' Weber with feedback from the spatialLIBD collaborators.
#'
#' @format A vector of length 9 with colors for Layers 1 through 9, WM, NA and
#' a special WM2 that is present in some of the unsupervised clustering
#' results.
"libd_layer_colors"

#' Cell cluster t-statistics from Tran et al
#'
#' Using the DLPFC snRNA-seq data from Matthew N Tran et al we computed
#' specificity t-statistics for the cell clusters. This is a subset of them
#' used in examples such as in [layer_stat_cor_plot()].
#'
#' @format A matrix with 692 rows and 31 variables where each column is
#' a given cell cluster from Tran et al and each row is one gene. The row names
#' are Ensembl gene IDs which are used by [layer_stat_cor()] to match to
#' our modeling results.
#'
#' @source \url{https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R}
#' and
#' \url{https://github.com/LieberInstitute/spatialLIBD/blob/master/dev/02_dev.R#L107-L194}.
"tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer"
