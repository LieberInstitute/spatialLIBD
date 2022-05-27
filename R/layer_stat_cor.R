#' Layer modeling correlation of statistics
#'
#' @param stats A data.frame where the row names are Ensembl gene IDs, the
#' column names are labels for clusters of cells or cell types, and where
#' each cell contains the given statistic for that gene and cell type. These
#' statistics should be computed similarly to the modeling results from
#' the data we provide. For example, like the `enrichment` t-statistics that
#' are derived from comparing one layer against the rest. The `stats` will be
#' matched and then correlated with our statistics.
#' @inheritParams sig_genes_extract
#' @param top_n An `integer(1)` specifying whether to filter to the top n marker
#' genes. The default is `NULL` in which case no filtering is done.
#'
#' @return A correlation matrix between `stats` and our statistics using only
#' the Ensembl gene IDs present in both tables. The columns are sorted using
#' a hierarchical cluster.
#'
#' @export
#' @importFrom stats cor dist hclust
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @family Layer correlation functions
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
#' ## Explore the correlation matrix
#' head(cor_stats_layer[, seq_len(3)])
#' summary(cor_stats_layer)
#'
#' ## Repeat with top_n set to 10
#' summary(layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     modeling_results,
#'     model_type = "enrichment",
#'     top_n = 10
#' ))
layer_stat_cor <-
    function(stats,
    modeling_results = fetch_data(type = "modeling_results"),
    model_type = names(modeling_results)[1],
    reverse = FALSE,
    top_n = NULL) {
        model_results <- modeling_results[[model_type]]

        tstats <-
            model_results[, grep("[f|t]_stat_", colnames(model_results))]
        colnames(tstats) <-
            gsub("[f|t]_stat_", "", colnames(tstats))

        if (reverse) {
            tstats <- tstats * -1
            colnames(tstats) <-
                vapply(strsplit(colnames(tstats), "-"), function(x) {
                    paste(rev(x), collapse = "-")
                }, character(1))
        }

        ## Subset to the 'top_n' marker genes if specified
        if (!is.null(top_n)) {
            stopifnot(top_n < nrow(tstats) & top_n > 0)
            ## Adapted from
            ## https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/ad_snRNAseq_recast.R#L207-L213
            top_n_index <- unique(as.vector(apply(tstats, 2, function(t) {
                order(t, decreasing = TRUE)[seq_len(top_n)]
            })))
            ## Subset to top n marker genes
            model_results <- model_results[top_n_index, , drop = FALSE]
            tstats <- tstats[top_n_index, , drop = FALSE]
        }

        ## Adapted from https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
        mm <- match(model_results$ensembl, rownames(stats))
        if(all(is.na(mm))) {
            stop("It looks like 'stats' does not have ENSEMBL gene names on the rownames or none of the genes are matching.", call. = FALSE)
        }

        tstats <- tstats[!is.na(mm), ]
        external_stats <- stats[mm[!is.na(mm)], ]

        ## Compute correlation
        cor_res <- cor(external_stats, tstats)

        ## Re-order just in case the layer names match our names
        if (identical(colnames(tstats), c("WM", paste0("Layer", seq_len(6))))) {
            cor_res <- cor_res[, c(1, seq(7, 2)), drop = FALSE]
        }

        ## Re-order the rows
        dd <- dist(1 - cor_res)
        hc <- hclust(dd)
        cor_res <- cor_res[hc$order, , drop = FALSE]
        return(cor_res)
    }
