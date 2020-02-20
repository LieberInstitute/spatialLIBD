#' Title
#'
#' @param n
#' @param modeling_results
#' @param model_type
#' @param reverse
#' @param sce_layer
#'
#' @return
#' @references Adapted from
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity_functions.R
#' @export
#' @importFrom rafalib splitit
#'
#' @examples
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#' ori_sce_layer <- fetch_data(type = 'sce_layer')
#'
#' ## anova top 10 genes
#' sig_genes_extract(
#'     modeling_results = ori_modeling_results,
#'     sce_layer = ori_sce_layer
#' )
#'
#' sig_genes_extract(
#'     modeling_results = ori_modeling_results,
#'     sce_layer = ori_sce_layer,
#'     n = nrow(ori_sce_layer)
#' )
#'
sig_genes_extract <- function(
    n = 10,
    modeling_results = fetch_data(type = 'modeling_results'),
    model_type = names(modeling_results)[1],
    reverse = FALSE,
    sce_layer = fetch_data(type = 'sce_layer')) {

    model_results <- modeling_results[[model_type]]

    tstats <-
        model_results[, grep('[f|t]_stat_', colnames(model_results))]
    colnames(tstats) <- gsub('[f|t]_stat_', '', colnames(tstats))

    if(reverse) {
        tstats <- tstats * -1
        colnames(tstats) <- sapply(strsplit(colnames(tstats), '-'), function(x) paste(rev(x), collapse = '-'))
    }

    pvals <-
        model_results[, grep('p_value_', colnames(model_results))]
    fdrs <- model_results[, grep('fdr_', colnames(model_results))]

    sig_genes <- apply(tstats, 2, function(x) {
        rowData(sce_layer)$gene_name[order(x, decreasing = TRUE)[seq_len(n)]]
    })

    sig_i <- apply(tstats, 2, function(x) {
        order(x, decreasing = TRUE)[seq_len(n)]
    })
    sig_genes_tstats <-
        sapply(seq_len(ncol(sig_i)), function(i) {
            tstats[sig_i[, i], i]
        })
    sig_genes_pvals <-
        sapply(seq_len(ncol(sig_i)), function(i) {
            pvals[sig_i[, i], i]
        })
    sig_genes_fdr <-
        sapply(seq_len(ncol(sig_i)), function(i) {
            fdrs[sig_i[, i], i]
        })
    dimnames(sig_genes_fdr) <-
        dimnames(sig_genes_tstats) <-
        dimnames(sig_genes_pvals) <- dimnames(sig_genes)

    ## Combine into a long format table
    sig_genes_tab <- data.frame(
        top = rep(seq_len(n), n = ncol(tstats)),
        model_type = model_type,
        test = rep(colnames(sig_genes), each = n),
        gene = as.character(sig_genes),
        stat = as.numeric(sig_genes_tstats),
        pval = as.numeric(sig_genes_pvals),
        fdr = as.numeric(sig_genes_fdr),
        gene_index = as.integer(sig_i),
        stringsAsFactors = FALSE
    )
    sig_genes_tab$ensembl <-
        rownames(sce_layer)[sig_genes_tab$gene_index]

    ## Add gene marker labels
    # sig_genes_tab <-
    #     cbind(sig_genes_tab, gene_ann(sig_genes_tab$gene))
    rownames(sig_genes_tab) <- NULL

    return(sig_genes_tab)
}
