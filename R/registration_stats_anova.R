#' Spatial registration: compute ANOVA statistics
#'
#' This function computes the gene ANOVA F-statistics (at least one group is
#' different from the rest). These F-statistics can be used for spatial
#' registration with `layer_stat_cor()` and related functions. Although, they
#' are more typically used for identifying ANOVA-marker genes.
#'
#' @inheritParams registration_stats_enrichment
#' @param prefix A `character(1)` specifying the prefix to use for the
#' F-statistics column. This is particularly useful if you will run this
#' function more than once and want to be able to merge the results.
#'
#' @return A `data.frame()` with the ANOVA statistical results. This is
#' similar to `fetch_data("modeling_results")$anova`.
#' @export
#' @importFrom limma lmFit eBayes topTable
#' @family spatial registration and statistical modeling functions.
#'
#' @examples
#' example("registration_block_cor", package = "spatialLIBD")
#' results_anova <- registration_stats_anova(sce_pseudo,
#'     block_cor, "age",
#'     gene_ensembl = "ensembl", gene_name = "gene_name", prefix = "example"
#' )
#' head(results_anova)
#'
#' ## Note that you can merge multiple of these data.frames if you run this
#' ## function for different sets. For example, maybe you drop one group
#' ## before pseudo-bulking if you know that there are many differences between
#' ## that group and others. For example, we have dropped the white matter (WM)
#' ## prior to computing ANOVA F-statistics.
registration_stats_anova <-
    function(sce_pseudo,
    block_cor,
    covars = NULL,
    var_registration = "registration_variable",
    var_sample_id = "registration_sample_id",
    gene_ensembl = NULL,
    gene_name = NULL,
    prefix = "") {
        if (is.null(covars)) {
            mat_formula <- eval(str2expression(paste("~", var_registration)))
        } else {
            mat_formula <- eval(str2expression(paste("~", var_registration, "+", paste(covars, collapse = " + "))))
        }
        mod <- model.matrix(mat_formula, data = colData(sce_pseudo))
        colnames(mod) <- gsub("^registration_variable", "", colnames(mod))

        message(Sys.time(), " computing F-statistics")
        x <- limma::eBayes(limma::lmFit(
            logcounts(sce_pseudo),
            design = mod,
            block = sce_pseudo[[var_sample_id]],
            correlation = block_cor
        ))

        ## Compute F-statistics
        top <- limma::topTable(
            x,
            coef = seq(2, ncol(x$coefficients), by = 1),
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
        colnames(results_anova) <- paste0(prefix, "_", colnames(results_anova))

        ## Add gene info
        results_anova$ensembl <-
            rowData(sce_pseudo)[[gene_ensembl]]
        results_anova$gene <- rowData(sce_pseudo)[[gene_name]]

        ## Done!
        return(results_anova)
    }
