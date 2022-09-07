#' Title
#'
#' @param sce_pseudo
#' @param registration_model
#' @param var_sample_id
#'
#' @return
#' @export
#'
#' @examples
#' block_cor <- registration_block_cor(sce_pseudo, mod)
registration_block_cor <- function(sce_pseudo, registration_model, var_sample_id = "registration_sample_id") {

    ## get duplicate correlation #http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/limma/html/dupcor.html
    message(Sys.time(), " run duplicateCorrelation()")
    corfit <- limma::duplicateCorrelation(logcounts(sce_pseudo), registration_model,
        block = sce_pseudo[[var_sample_id]]
    )
    corfit$consensus.correlation
}
