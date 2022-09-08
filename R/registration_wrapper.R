#' Spatial registration: wrapper function
#'
#' This function is provided for convenience. It runs all the functions
#' required for computing the `modeling_results`. This can be useful for
#' finding marker genes on a new spatially-resolved transcriptomics dataset
#' and thus using it for `run_app()`. The results from this function can also be
#' used for performing spatial registration through `layer_stat_cor()` and
#' related functions of sc/snRNA-seq datasets.
#'
#' @inheritParams registration_pseudobulk
#' @inheritParams registration_stats_enrichment
#' @inheritParams registration_stats_anova
#' @param min_ncells An `integer(1)` greater than 0 specifying the minimum
#' number of cells (for scRNA-seq) or spots (for spatial) that are combined
#' when pseudo-bulking. Pseudo-bulked samples with less than `min_ncells` on
#' `sce_pseudo$ncells` will be dropped.
#'
#' @return A `list()` of `data.frame()` with the statistical results. This is
#' similar to `fetch_data("modeling_results")`.
#' @export
#' @family spatial registration and statistical modeling functions
#'
#' @examples
#' ## Ensure reproducibility of example data
#' set.seed(20220907)
#'
#' ## Generate example data
#' sce <- scuttle::mockSCE()
#'
#' ## Add some sample IDs
#' sce$sample_id <- sample(LETTERS[1:5], ncol(sce), replace = TRUE)
#'
#' ## Add a sample-level covariate: age
#' ages <- rnorm(5, mean = 20, sd = 4)
#' names(ages) <- LETTERS[1:5]
#' sce$age <- ages[sce$sample_id]
#'
#' ## Add gene-level information
#' rowData(sce)$ensembl <- paste0("ENSG", seq_len(nrow(sce)))
#' rowData(sce)$gene_name <- paste0("gene", seq_len(nrow(sce)))
#'
#' ## Compute all modeling results
#' example_modeling_results <- registration_wrapper(
#'     sce,
#'     "Treatment", "sample_id", c("age"), "ensembl", "gene_name", "wrapper"
#' )
registration_wrapper <-
    function(sce,
    var_registration,
    var_sample_id,
    covars = NULL,
    gene_ensembl = NULL,
    gene_name = NULL,
    prefix = "",
    min_ncells = NULL) {
        sce_pseudo <-
            registration_pseudobulk(sce,
                var_registration = var_registration,
                var_sample_id = var_sample_id
            )

        if (!is.null(min_ncells)) {
            message(
                Sys.time(),
                " dropping ",
                sum(sce_pseudo$ncells < min_ncells),
                " pseudo-bulked samples that are below 'min_ncells'."
            )
            sce_pseudo <- sce_pseudo[, sce_pseudo$ncells >= min_ncells]
        }
        registration_mod <-
            registration_model(sce_pseudo, covars = covars)
        block_cor <-
            registration_block_cor(sce_pseudo, registration_model = registration_mod)

        results_enrichment <-
            registration_stats_enrichment(
                sce_pseudo,
                block_cor = block_cor,
                covars = covars,
                gene_ensembl = gene_ensembl,
                gene_name = gene_name
            )
        results_pairwise <-
            registration_stats_pairwise(
                sce_pseudo,
                registration_model = registration_mod,
                block_cor = block_cor,
                gene_ensembl = gene_ensembl,
                gene_name = gene_name
            )
        results_anova <-
            registration_stats_anova(
                sce_pseudo,
                block_cor = block_cor,
                covars = covars,
                gene_ensembl = gene_ensembl,
                gene_name = gene_name,
                prefix = prefix
            )

        modeling_results <- list(
            "anova" = results_anova,
            "enrichment" = results_enrichment,
            "pairwise" = results_pairwise
        )

        return(modeling_results)
    }
