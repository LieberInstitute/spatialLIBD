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
#' if (!exists('modeling_results'))
#'     modeling_results <- fetch_data(type = 'modeling_results')
#'
#' ## Check the object
#' xx <- check_modeling_results(modeling_results)
#'

check_modeling_results <- function(modeling_results) {

    ## Should be a list
    stopifnot(is(modeling_results, 'list'))

    stopifnot(all(
        c('enrichment', 'pairwise', 'anova') %in% names(modeling_results)
    ))

    ## All the model tables contain the ensembl column
    stopifnot(all(
        vapply(modeling_results, function(x)
            'ensembl' %in% colnames(x), logical(1))
    ))

    ## The following column prefixes are present in each of the model tables
    expected_column_name_starts <-
        c('^[f|t]_stat_', '^p_value_', '^fdr_')
    stopifnot(all(vapply(modeling_results, function(model_table) {
        all(vapply(expected_column_name_starts, function(expected_prefix)
            any(
                grepl(expected_prefix, colnames(model_table))
            ), logical(1)))
    }, logical(1))))

    ## Done!
    return(modeling_results)
}
