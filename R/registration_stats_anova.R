#' Spatial registration: compute ANOVA statistics
#'
#' This function computes the gene ANOVA F-statistics (at least one group is
#' different from the rest). These F-statistics can be used for spatial
#' registration with `layer_stat_cor()` and related functions. Although, they
#' are more typically used for identifying ANOVA-marker genes.
#'
#' @inheritParams registration_stats_enrichment
#' @param suffix A `character(1)` specifying the suffix to use for the
#' F-statistics column. This is particularly useful if you will run this
#' function more than once and want to be able to merge the results.
#'
#' @return A `data.frame()` with the ANOVA statistical results. This is
#' similar to `fetch_data("modeling_results")$anova`.
#' @export
#' @importFrom limma lmFit eBayes topTable
#' @family spatial registration and statistical modeling functions
#'
#' @examples
#' example("registration_block_cor", package = "spatialLIBD")
#' results_anova <- registration_stats_anova(sce_pseudo,
#'     block_cor, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name", suffix = "example"
#' )
#' head(results_anova)
#'
#' ## Specifying `block_cor = NaN` then ignores the correlation structure
#' results_anova_nan <- registration_stats_anova(sce_pseudo,
#'     block_cor = NaN, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name", suffix = "example"
#' )
#' head(results_anova_nan)
#'
#' ## Note that you can merge multiple of these data.frames if you run this
#' ## function for different sets. For example, maybe you drop one group
#' ## before pseudo-bulking if you know that there are many differences between
#' ## that group and others. For example, we have dropped the white matter (WM)
#' ## prior to computing ANOVA F-statistics.
#'
#' ## no covariates
#' results_anova_nocovar <- registration_stats_anova(sce_pseudo,
#'     block_cor,
#'     covars = NULL,
#'     gene_ensembl = "ensembl", gene_name = "gene_name", suffix = "nocovar"
#' )
#' head(results_anova_nocovar)
#'
#' ## Merge both results into a single data.frame, thanks to having different
#' ## 'suffix' values.
#' results_anova_merged <- merge(results_anova, results_anova_nocovar)
#' head(results_anova_merged)
registration_stats_anova <-
    function(sce_pseudo,
    block_cor,
    covars = NULL,
    var_registration = "registration_variable",
    var_sample_id = "registration_sample_id",
    gene_ensembl = NULL,
    gene_name = NULL,
    suffix = "") {
        if (is.null(covars)) {
            mat_formula <- eval(str2expression(paste("~", var_registration)))
        } else {
            mat_formula <- eval(str2expression(paste("~", var_registration, "+", paste(covars, collapse = " + "))))
        }
        mod <- model.matrix(mat_formula, data = colData(sce_pseudo))
        coef_registration <- grep(paste0("^", var_registration), colnames(mod))
        if (length(coef_registration) <= 1) {
            stop("You need 'var_registration' to have at least 3 different values to compute an F-statistic.", call. = FALSE)
        }
        colnames(mod) <- gsub(paste0("^", var_registration), "", colnames(mod))

        message(Sys.time(), " computing F-statistics")

        if (is.finite(block_cor)) {
            x <- limma::eBayes(
                limma::lmFit(
                    logcounts(sce_pseudo),
                    design = mod,
                    block = sce_pseudo[[var_sample_id]],
                    correlation = block_cor
                )
            )
        } else {
            x <- limma::eBayes(limma::lmFit(logcounts(sce_pseudo),
                design = mod
            ))
        }


        ## Compute F-statistics
        top <- limma::topTable(
            x,
            coef = coef_registration,
            sort.by = "none",
            number = length(x$F)
        )

        ## Reformat results
        results_anova <- data.frame(
            "f_stat" = top$F,
            "p_value" = top$P.Value,
            "fdr" = top$adj.P.Val,
            "AveExpr" = top$AveExpr
        )
        colnames(results_anova) <- paste0(colnames(results_anova), "_", suffix)

        ## Add gene info
        results_anova$ensembl <-
            rowData(sce_pseudo)[[gene_ensembl]]
        results_anova$gene <- rowData(sce_pseudo)[[gene_name]]

        ## Done!
        return(results_anova)
    }
