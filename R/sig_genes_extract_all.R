#' Title
#'
#' @param n
#' @param modeling_results
#' @param sce_layer
#'
#' @return
#' @export
#'
#' @examples
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#' ori_sce_layer <- fetch_data(type = 'sce_layer')
#'
#' ## anova top 10 genes
#' sig_genes_extract_all(
#'     modeling_results = ori_modeling_results,
#'     sce_layer = ori_sce_layer
#' )
#'
sig_genes_extract_all <- function(n = 10,
    modeling_results = fetch_data(type = 'modeling_results'),
    sce_layer = fetch_data(type = 'sce_layer')) {


    sig_genes_specificity <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = 'specificity',
            sce_layer = sce_layer
        )
    sig_genes_pairwise <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = 'pairwise',
            sce_layer = sce_layer
        )
    sig_genes_pairwise_rev <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = 'pairwise',
            sce_layer = sce_layer,
            reverse = TRUE
        )
    sig_genes_anova <-
        sig_genes_extract(
            n = n,
            modeling_results = modeling_results,
            model_type = 'anova',
            sce_layer = sce_layer
        )

    sig_genes <- DataFrame(rbind(
        sig_genes_specificity,
        sig_genes_pairwise,
        sig_genes_pairwise_rev,
        sig_genes_anova
    ))

    sig_genes_unique <- rafalib::splitit(sig_genes$ensembl)

    sig_genes$in_rows <-
        IntegerList(sig_genes_unique)[sig_genes$ensembl]


    sig_genes_unique_top20 <- rafalib::splitit(sig_genes$ensembl[sig_genes$top <= 20])
    sig_genes$in_rows_top20 <-  IntegerList(lapply(sig_genes$in_rows, function(x)
            NULL))
    sig_genes$in_rows_top20[names(sig_genes_unique_top20)] <- IntegerList(sig_genes_unique_top20)

    sig_genes$results <-
        CharacterList(lapply(sig_genes$in_rows_top20 , function(x) {
            if(length(x) == 0) return(NULL)
            paste0(sig_genes$test[x], '_top', sig_genes$top[x])
        }))


    return(sig_genes)

}
