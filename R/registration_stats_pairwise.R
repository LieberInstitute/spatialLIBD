#' Spatial registration: compute pairwise statistics
#'
#' This function computes the gene pairwise t-statistics (one group > another,
#' for all combinations). These t-statistics can be used for spatial
#' registration with `layer_stat_cor()` and related functions. Although, they
#' are more typically used for identifying pairwise-marker genes.
#'
#' @inheritParams registration_stats_enrichment
#' @inheritParams registration_block_cor
#'
#' @return A `data.frame()` with the pairwise statistical results. This is
#' similar to `fetch_data("modeling_results")$pairwise`.
#' @export
#' @family spatial registration and statistical modeling functions
#' @importFrom limma lmFit eBayes makeContrasts contrasts.fit
#' @importFrom utils combn
#' @importFrom stats p.adjust
#'
#' @examples
#' example("registration_block_cor", package = "spatialLIBD")
#' results_pairwise <- registration_stats_pairwise(sce_pseudo,
#'     registration_mod, block_cor,
#'     gene_ensembl = "ensembl", gene_name = "gene_name"
#' )
#' head(results_pairwise)
#'
#' ## Specifying `block_cor = NaN` then ignores the correlation structure
#' results_pairwise_nan <- registration_stats_pairwise(sce_pseudo,
#'     registration_mod,
#'     block_cor = NaN,
#'     gene_ensembl = "ensembl", gene_name = "gene_name"
#' )
#' head(results_pairwise_nan)
registration_stats_pairwise <-
    function(
        sce_pseudo,
        registration_model,
        block_cor,
        var_registration = "registration_variable",
        var_sample_id = "registration_sample_id",
        gene_ensembl = NULL,
        gene_name = NULL) {
        ## Identify which are the pairwise columns of interest (aka, don't use
        ## the sample-level covariates we are adjusting for) and then
        ## shorten the names
        regis_cols <- grep(paste0("^", var_registration), colnames(registration_model))
        colnames(registration_model) <- gsub(paste0("^", var_registration), "", colnames(registration_model))
        regis_combs <- combn(colnames(registration_model)[regis_cols], 2)

        message(Sys.time(), " running the baseline pairwise model")
        if (is.finite(block_cor)) {
            fit <-
                limma::lmFit(
                    logcounts(sce_pseudo),
                    design = registration_model,
                    block = sce_pseudo[[var_sample_id]],
                    correlation = block_cor
                )
        } else {
            fit <-
                limma::lmFit(logcounts(sce_pseudo),
                    design = registration_model
                )
        }
        eb <- limma::eBayes(fit)

        ## Define the contrasts for each group vs another one
        message(Sys.time(), " computing pairwise statistics")
        regis_constrasts <- apply(regis_combs, 2, function(x) {
            z <- paste(x, collapse = "-")
            limma::makeContrasts(contrasts = z, levels = registration_model)
        })
        rownames(regis_constrasts) <- colnames(registration_model)
        colnames(regis_constrasts) <-
            apply(regis_combs, 2, paste, collapse = "-")
        eb_contrasts <- limma::eBayes(limma::contrasts.fit(fit, regis_constrasts))

        ## Re-format results
        pvals_contrasts <- eb_contrasts$p.value
        fdrs_contrasts <- apply(pvals_contrasts, 2, p.adjust, "fdr")

        results_pairwise <-
            f_merge(p = pvals_contrasts, fdr = fdrs_contrasts, t = eb_contrasts$t, logFC = eb_contrasts$coefficients)

        ## Add gene info
        results_pairwise$ensembl <-
            rowData(sce_pseudo)[[gene_ensembl]]
        results_pairwise$gene <- rowData(sce_pseudo)[[gene_name]]

        ## Done!
        return(results_pairwise)
    }
