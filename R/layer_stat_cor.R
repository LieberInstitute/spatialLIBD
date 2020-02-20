#' Title
#'
#' @param stats
#' @param modeling_results
#' @param model_type
#' @param reverse
#'
#' @return
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#'
#' @examples
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' cor_stats_layer <- layer_stat_cor(
#'     tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
#'     ori_modeling_results,
#'     'specificity'
#' )
#' head(cor_stats_layer[, 1:3])
#'
layer_stat_cor <-
    function(stats,
        modeling_results = fetch_data(type = 'modeling_results'),
        model_type = names(modeling_results)[1],
        reverse = FALSE) {
        model_results <- modeling_results[[model_type]]

        tstats <-
            model_results[, grep('[f|t]_stat_', colnames(model_results))]
        colnames(tstats) <-
            gsub('[f|t]_stat_', '', colnames(tstats))

        if (reverse) {
            tstats <- tstats * -1
            colnames(tstats) <-
                sapply(strsplit(colnames(tstats), '-'), function(x)
                    paste(rev(x), collapse = '-'))
        }

        ## Adapted from https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R
        mm <- match(model_results$ensembl, rownames(stats))

        tstats <- tstats[!is.na(mm),]
        external_stats <- stats[mm[!is.na(mm)],]

        ## Compute correlation
        cor_res <- cor(external_stats, tstats)

        ## Re-order just in case the layer names match our names
        if (identical(colnames(tstats), c('WM', paste0('Layer', 1:6)))) {
            cor_res <- cor_res[, c(1, 7:2), drop = FALSE]
        }
        return(cor_res)

    }
