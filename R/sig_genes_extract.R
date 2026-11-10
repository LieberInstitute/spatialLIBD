#' Extract significant genes
#'
#' From the layer-level modeling results, this function extracts the top `n`
#' significant genes. This is the workhorse function used by
#' [sig_genes_extract_all()] through which we obtain the information that can
#' then be used by functions such as [layer_boxplot()] for constructing
#' informative titles.
#'
#' @param n The number of the top ranked genes to extract.
#' @param modeling_results Defaults to the output of
#' `fetch_data(type = 'modeling_results')`. This is a list of tables with the
#' columns `f_stat_*` or `t_stat_*` as well as `p_value_*` and `fdr_*` plus
#' `ensembl`. The column name is used to extract the statistic results, the
#' p-values, and the FDR adjusted p-values. Then the `ensembl` column is used
#' for matching in some cases. See [fetch_data()] for more details. Typically
#' this is the set of reference statistics used in `layer_stat_cor()`.
#' @param model_type A named element of the `modeling_results` list. By default
#' that is either `enrichment` for the model that tests one human brain layer
#' against the rest (one group vs the rest), `pairwise` which compares two
#' layers (groups) denoted by `layerA-layerB` such that `layerA` is greater
#' than `layerB`, and `anova` which determines if any layer (group) is different
#' from the rest adjusting for the mean expression level. The statistics for
#' `enrichment` and `pairwise` are t-statistics while the `anova` model ones
#' are F-statistics.
#' @param reverse A `logical(1)` indicating whether to multiply by `-1` the
#' input statistics and reverse the `layerA-layerB` column names (using the `-`)
#' into `layerB-layerA`.
#' @param sce_layer Defaults to the output of
#' `fetch_data(type = 'sce_layer')`. This is a
#' \linkS4class{SingleCellExperiment}
#' object with the spot-level Visium data compressed via pseudo-bulking to the
#' layer-level (group-level) resolution. See [fetch_data()] for more details.
#' @param gene_name A `character(1)` specifying the `rowData(sce_layer)`
#' column with the gene names that match the `rownames(modeling_results)`.
#' Defaults to `"gene_name"`.
#'
#' @return A `data.frame()` with the top `n` significant genes
#' (as ordered by their statistics in decreasing order) in long format. The
#' specific columns are described further in the vignette.
#'
#' @references Adapted from
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity_functions.R
#' @export
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
#' ## anova top 10 genes
#' sig_genes_extract(
#'     modeling_results = modeling_results,
#'     sce_layer = sce_layer
#' )
#'
#' ## Extract all genes
#' sig_genes_extract(
#'     modeling_results = modeling_results,
#'     sce_layer = sce_layer,
#'     n = nrow(sce_layer)
#' )
sig_genes_extract <- function(
    n = 10,
    modeling_results = fetch_data(type = "modeling_results"),
    model_type = names(modeling_results)[1],
    reverse = FALSE,
    sce_layer = fetch_data(type = "sce_layer"),
    gene_name = "gene_name"
) {
    ## check variables are present
    stopifnot(model_type %in% names(modeling_results))
    stopifnot(gene_name %in% colnames(rowData(sce_layer)))

    model_results <- modeling_results[[model_type]]

    tstats <-
        model_results[,
            grep("[f|t]_stat_", colnames(model_results)),
            drop = FALSE
        ]
    colnames(tstats) <- gsub("[f|t]_stat_", "", colnames(tstats))

    if (reverse) {
        tstats <- tstats * -1
        colnames(tstats) <-
            vapply(
                strsplit(colnames(tstats), "-"),
                function(x) {
                    paste(rev(x), collapse = "-")
                },
                character(1)
            )
    }

    pvals <-
        model_results[, grep("p_value_", colnames(model_results)), drop = FALSE]
    fdrs <- model_results[, grep("fdr_", colnames(model_results)), drop = FALSE]
    logFC <- model_results[,
        grep("logFC_", colnames(model_results)),
        drop = FALSE
    ]

    sig_genes <- apply(tstats, 2, function(x) {
        rowData(sce_layer)[[gene_name]][order(x, decreasing = TRUE)[seq_len(n)]]
    })

    sig_i <- apply(tstats, 2, function(x) {
        order(x, decreasing = TRUE)[seq_len(n)]
    })
    sig_genes_tstats <-
        vapply(
            seq_len(ncol(sig_i)),
            function(i) {
                tstats[sig_i[, i], i]
            },
            numeric(n)
        )
    if (ncol(logFC) > 0) {
        sig_genes_logFC <-
            vapply(
                seq_len(ncol(sig_i)),
                function(i) {
                    logFC[sig_i[, i], i]
                },
                numeric(n)
            )
        dimnames(sig_genes_logFC) <- dimnames(sig_genes)
    } else {
        sig_genes_logFC <- NULL
    }

    sig_genes_pvals <-
        vapply(
            seq_len(ncol(sig_i)),
            function(i) {
                pvals[sig_i[, i], i]
            },
            numeric(n)
        )
    sig_genes_fdr <-
        vapply(
            seq_len(ncol(sig_i)),
            function(i) {
                fdrs[sig_i[, i], i]
            },
            numeric(n)
        )
    dimnames(sig_genes_fdr) <- dimnames(sig_genes_tstats) <- dimnames(
        sig_genes_pvals
    ) <- dimnames(sig_genes)

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
    if (!is.null(sig_genes_logFC)) {
        sig_genes_tab$logFC <- as.numeric(sig_genes_logFC)
    }
    sig_genes_tab$ensembl <-
        rownames(sce_layer)[sig_genes_tab$gene_index]

    ## Add gene marker labels
    # sig_genes_tab <-
    #     cbind(sig_genes_tab, gene_ann(sig_genes_tab$gene))
    rownames(sig_genes_tab) <- NULL

    return(sig_genes_tab)
}
