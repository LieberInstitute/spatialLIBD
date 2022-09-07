#' Spatial registration: block correlation
#'
#' This function computes the block correlation using the sample ID as the
#' blocking factor. This takes into account that cells in scRNA-seq data or
#' spots in spatially-resolved transcriptomics data from Visium (or similar)
#' have a sample ID batch effect.
#'
#' @param registration_model The output from `registration_model()`.
#' @param var_sample_id A `character(1)` specifying the `colData(sce_pseudo)`
#' variable with the sample ID.
#' @inheritParams registration_model
#'
#' @return A `numeric(1)` with the block correlation at the sample ID level.
#' @export
#' @importFrom limma duplicateCorrelation
#' @family spatial registration and statistical modeling functions.
#'
#' @examples
#' example("registration_model", package = "spatialLIBD")
#' block_cor <- registration_block_cor(sce_pseudo, registration_mod)
registration_block_cor <- function(sce_pseudo, registration_model, var_sample_id = "registration_sample_id") {

    ## get duplicate correlation #http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/limma/html/dupcor.html
    message(Sys.time(), " run duplicateCorrelation()")
    corfit <- limma::duplicateCorrelation(logcounts(sce_pseudo), registration_model,
        block = sce_pseudo[[var_sample_id]]
    )
    corfit$consensus.correlation
}
