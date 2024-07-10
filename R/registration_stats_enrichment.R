#' Spatial registration: compute enrichment statistics
#'
#' This function computes the gene enrichment t-statistics (one group > the
#' rest). These t-statistics are the ones typically used for spatial
#' registration with `layer_stat_cor()` and related functions.
#'
#' @param block_cor A `numeric(1)` computed with `registration_block_cor()`.
#' @param gene_ensembl A `character(1)` specifying the `rowData(sce_pseudo)`
#' column with the ENSEMBL gene IDs. This will be used by `layer_stat_cor()`.
#' @param gene_name A `character(1)` specifying the `rowData(sce_pseudo)`
#' column with the gene names (symbols).
#' @inheritParams registration_model
#' @inheritParams registration_block_cor
#'
#' @return A `data.frame()` with the enrichment statistical results. This is
#' similar to `fetch_data("modeling_results")$enrichment`.
#' @export
#' @importFrom limma lmFit eBayes
#' @importFrom stats model.matrix p.adjust
#' @family spatial registration and statistical modeling functions
#'
#' @examples
#' example("registration_block_cor", package = "spatialLIBD")
#' results_enrichment <- registration_stats_enrichment(sce_pseudo,
#'     block_cor, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name"
#' )
#' head(results_enrichment)
#'
#' ## Specifying `block_cor = NaN` then ignores the correlation structure
#' results_enrichment_nan <- registration_stats_enrichment(sce_pseudo,
#'     block_cor = NaN, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name"
#' )
#' head(results_enrichment_nan)
registration_stats_enrichment <-
    function(sce_pseudo,
    block_cor,
    covars = NULL,
    var_registration = "registration_variable",
    var_sample_id = "registration_sample_id",
    gene_ensembl = NULL,
    gene_name = NULL) {
        ## For each cluster, test it against the rest
        cluster_idx <- split(seq(along = sce_pseudo[[var_registration]]), sce_pseudo[[var_registration]])

        message(Sys.time(), " computing enrichment statistics")
        eb0_list_cluster <- lapply(cluster_idx, function(x) {
            res <- rep(0, ncol(sce_pseudo))
            res[x] <- 1
            if (!is.null(covars)) {
                res_formula <-
                    eval(str2expression(paste(
                        "~", "res", "+", paste(covars, collapse = " + ")
                    )))
            } else {
                res_formula <- eval(str2expression(paste("~", "res")))
            }
            m <- model.matrix(res_formula, data = colData(sce_pseudo))

            if (is.finite(block_cor)) {
                res <- limma::eBayes(limma::lmFit(
                    logcounts(sce_pseudo),
                    design = m,
                    block = sce_pseudo[[var_sample_id]],
                    correlation = block_cor
                ))
            } else {
                res <- limma::eBayes(limma::lmFit(logcounts(sce_pseudo),
                    design = m
                ))
            }
            return(res)
        })


        message(Sys.time(), " extract and reformat enrichment results")

        ## Extract the p-values
        pvals0_contrasts_cluster <-
            sapply(eb0_list_cluster, function(x) {
                x$p.value[, 2, drop = FALSE]
            })
        rownames(pvals0_contrasts_cluster) <- rownames(sce_pseudo)

        ## Extract t-statistics
        t0_contrasts_cluster <- sapply(eb0_list_cluster, function(x) {
            x$t[, 2, drop = FALSE]
        })
        rownames(t0_contrasts_cluster) <- rownames(sce_pseudo)

        ## Extract logFC
        logFC_contrasts_cluster <- sapply(eb0_list_cluster, function(x) {
            x$coefficients[, 2, drop = FALSE]
        })
        rownames(logFC_contrasts_cluster) <- rownames(sce_pseudo)

        ## Compute FDRs
        fdrs0_contrasts_cluster <-
            apply(pvals0_contrasts_cluster, 2, p.adjust, "fdr")

        ## Merge into one data.frame
        results_specificity <-
            f_merge(p = pvals0_contrasts_cluster, fdr = fdrs0_contrasts_cluster, t = t0_contrasts_cluster, logFC = logFC_contrasts_cluster)

        ## Add gene info
        results_specificity$ensembl <-
            rowData(sce_pseudo)[[gene_ensembl]]
        results_specificity$gene <- rowData(sce_pseudo)[[gene_name]]

        ## Done!
        return(results_specificity)
    }

## Helper function
f_merge <- function(p, fdr, t, logFC) {
    ## Add some prefixes to the columns that will be recognized by
    ## other spatialLIBD functions
    colnames(p) <- paste0("p_value_", colnames(p))
    colnames(fdr) <- paste0("fdr_", colnames(fdr))
    colnames(t) <- paste0("t_stat_", colnames(t))
    colnames(logFC) <- paste0("logFC_", colnames(logFC))

    ## Merge into a data.frame
    res <- as.data.frame(cbind(t, p, fdr, logFC))
    return(res)
}
