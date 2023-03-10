#' Extract significant genes for all modeling results
#'
#' This function combines the output of [sig_genes_extract()] from all the
#' layer-level (group-level) modeling results and builds the data required for
#' functions such as [layer_boxplot()].
#'
#' @inheritParams sig_genes_extract
#'
#' @return A [DataFrame-class][S4Vectors::DataFrame-class] with the extracted
#' statistics in long format. The specific columns are described further in
#' the vignette.
#' @export
#' @importFrom S4Vectors DataFrame
#' @importFrom IRanges IntegerList CharacterList
#' @family Layer modeling functions
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists("modeling_results")) {
#'     modeling_results <- fetch_data(type = "modeling_results")
#' }
#' if (!exists("sce_layer")) sce_layer <- fetch_data(type = "sce_layer")
#'
#' ## top 10 genes for all models
#' sig_genes_extract_all(
#'     modeling_results = modeling_results,
#'     sce_layer = sce_layer
#' )
sig_genes_extract_all <- function(
        n = 10,
        modeling_results = fetch_data(type = "modeling_results"),
        sce_layer = fetch_data(type = "sce_layer")) {
    ## Run checks since this function is run by default by run_app()
    ## before the checks have been run elsewhere
    sce_layer <- check_sce_layer(sce_layer)
    modeling_results <- check_modeling_results(modeling_results)

    sig_genes_enrichment <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = "enrichment",
            sce_layer = sce_layer
        )
    sig_genes_pairwise <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = "pairwise",
            sce_layer = sce_layer
        )
    sig_genes_pairwise_rev <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = "pairwise",
            sce_layer = sce_layer,
            reverse = TRUE
        )
    sig_genes_anova <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = "anova",
            sce_layer = sce_layer
        )
    if ("logFC" %in% colnames(sig_genes_enrichment)) {
        sig_genes_anova$logFC <- NA
    }

    sig_genes <- DataFrame(
        rbind(
            sig_genes_enrichment,
            sig_genes_pairwise,
            sig_genes_pairwise_rev,
            sig_genes_anova
        )
    )

    ## from rafalib
    splitit <- function(x) {
        split(seq(along.with = x), x)
    }
    sig_genes_unique <- splitit(sig_genes$ensembl)

    sig_genes$in_rows <-
        IntegerList(sig_genes_unique)[sig_genes$ensembl]


    sig_genes_unique_top20 <-
        split(which(sig_genes$top <= 20), sig_genes$ensembl[sig_genes$top <= 20])
    sig_genes$in_rows_top20 <-
        IntegerList(lapply(sig_genes$in_rows, function(x) {
            NULL
        }))
    sig_genes$in_rows_top20[names(sig_genes_unique_top20)] <-
        IntegerList(sig_genes_unique_top20)

    sig_genes$results <-
        CharacterList(lapply(sig_genes$in_rows_top20, function(x) {
            if (length(x) == 0) {
                return(NULL)
            }
            paste0(sig_genes$test[x], "_top", sig_genes$top[x])
        }))[sig_genes$ensembl]


    return(sig_genes)
}
