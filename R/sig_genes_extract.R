#' Extract significant genes
#'
#' From the layer-level modeling results, this function extracts the top `n`
#' significant genes. This is the workhorse function used by
#' `sig_genes_extract_all()` through which we obtain the information that can
#' then be used by functions such as `layer_boxplot()` for constructing
#' informative titles.
#'
#' @param n The number of the top ranked genes to extract.
#' @param modeling_results Defaults to the output of
#' `fetch_data(type = 'modeling_results')`. This is a list of tables with the
#' columns `f_stat_*` or `t_stat_*` as well as `p_value_*` and `fdr_*` plus
#' `ensembl`. The column name is used to extract the statistic results, the
#' p-values, and the FDR adjusted p-values. Then the `ensembl` column is used
#' for matching in some cases. See `fetch_data()` for more details.
#' @param model_type A named element of the `modeling_results` list. By default
#' that is either `specificity` for the model that tests one human brain layer
#' against the rest (one group vs the rest), `pairwise` which compares two
#' layers (groups) denoted by `layerA-layerB` such that `layerA` is greater
#' than `layerB`, and `anova` which determines if any layer (group) is different
#' from the rest adjusting for the mean expression level. The statistics for
#' `specificity` and `pairwise` are t-statistics while the `anova` model ones
#' are F-statistics.
#' @param reverse A `logical(1)` indicating whether to multiply by `-1` the
#' input statistics and reverse the `layerA-layerB` column names (using the `-`)
#' into `layerB-layerA`.
#' @param sce_layer Defaults to the output of
#' `fetch_data(type = 'sce_layer')`. This is a
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class]
#' object with the spot-level Visium data compressed via pseudo-bulking to the
#' layer-level (group-level) resolution. See `fetch_data()` for more details.
#'
#' @return A `data.frame()` with the top `n` significant genes
#' (as ordered by their statistics in decreasing order) in long format.
#'
#' @references Adapted from
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity_functions.R
#' @export
#' @family Layer modeling functions
#'
#' @examples
#'
#' ## Obtain the necessary data
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#' ori_sce_layer <- fetch_data(type = 'sce_layer')
#'
#' ## anova top 10 genes
#' sig_genes_extract(
#'     modeling_results = ori_modeling_results,
#'     sce_layer = ori_sce_layer
#' )
#'
#' ## Extract all genes
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
