#' Check input modeling_results
#'
#' This function checks that the `modeling_results` object has the appropriate structure.
#' For more details please check the vignette documentation.
#'
#' @inheritParams run_app
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @family Check input functions
#'
#' @examples
#'
#' if (!exists('ori_modeling_results'))
#'     ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' ## Check the object
#' xx <- check_modeling_results(ori_modeling_results)
#'

check_modeling_results <- function(modeling_results) {

    ## Should be a list
    stopifnot(is(modeling_results, 'list'))

    stopifnot(all(
        c('enrichment', 'pairwise', 'anova') %in% names(modeling_results)
    ))

    ## All the model tables contain the ensembl column
    stopifnot(all(
        sapply(modeling_results, function(x)
            'ensembl' %in% colnames(x))
    ))

    ## The following column prefixes are present in each of the model tables
    expected_column_name_starts <-
        c('^[f|t]_stat_', '^p_value_', '^fdr_')
    stopifnot(all(sapply(modeling_results, function(model_table) {
        all(sapply(expected_column_name_starts, function(expected_prefix)
            any(
                grepl(expected_prefix, colnames(model_table))
            )))
    })))

    ## Done!
    return(modeling_results)
}
