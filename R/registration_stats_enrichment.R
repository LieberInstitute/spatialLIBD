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
#' @family spatial registration and statistical modeling functions.
#'
#' @examples
#' example("registration_block_cor", package = "spatialLIBD")
#' results_specificity <- registration_stats_enrichment(sce_pseudo,
#'     block_cor, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name"
#' )
#' head(results_specificity)
registration_stats_enrichment <-
    function(sce_pseudo,
    block_cor,
    covars = NULL,
    var_registration = "registration_variable",
    var_sample_id = "registration_sample_id",
    gene_ensembl = NULL,
    gene_name = NULL) {
        ## Next for each layer test that layer vs the rest
        cluster_idx <- split(seq(along = sce_pseudo[[var_registration]]), sce_pseudo[[var_registration]])

        message(Sys.time(), " run enrichment statistics")
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

            # josh suggested use top table as a wrapper because it makes the output of eBayes nicer

            limma::eBayes(
                limma::lmFit(
                    logcounts(sce_pseudo),
                    design = m,
                    block = sce_pseudo[[var_sample_id]],
                    correlation = block_cor
                )
            )
        })


        message(Sys.time(), " extract and reformat enrichment results")
        ##########
        ## Extract the p-values
        pvals0_contrasts_cluster <-
            sapply(eb0_list_cluster, function(x) {
                x$p.value[, 2, drop = FALSE]
            })
        rownames(pvals0_contrasts_cluster) <- rownames(sce_pseudo)

        t0_contrasts_cluster <- sapply(eb0_list_cluster, function(x) {
            x$t[, 2, drop = FALSE]
        })
        rownames(t0_contrasts_cluster) <- rownames(sce_pseudo)

        fdrs0_contrasts_cluster <-
            apply(pvals0_contrasts_cluster, 2, p.adjust, "fdr")

        results_specificity <-
            f_merge(p = pvals0_contrasts_cluster, fdr = fdrs0_contrasts_cluster, t = t0_contrasts_cluster)

        results_specificity$ensembl <-
            rowData(sce_pseudo)[[gene_ensembl]]
        results_specificity$gene <- rowData(sce_pseudo)[[gene_name]]
        return(results_specificity)
    }

## Helper function
f_merge <- function(p, fdr, t) {
    colnames(p) <- paste0("p_value_", colnames(p))
    colnames(fdr) <- paste0("fdr_", colnames(fdr))
    colnames(t) <- paste0("t_stat_", colnames(t))
    res <- as.data.frame(cbind(t, p, fdr))
    return(res)
}
