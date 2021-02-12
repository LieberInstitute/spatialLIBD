#' Create a data frame with for the visualization functions
#'
#' This function creates a data frame combining the `colData(spe)` columns and
#' the `spatialData(spe)` information from a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object. This makes it easier to use the functions such as `vis_gene_p()` and
#' `vis_clus_p()`.
#'
#' @param spe  A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object
#' created, such as one created by `sce_to_spe()`.
#'
#' @return A `data.frame` containing `colData(spe)` columns and
#' the `spatialData(spe)` data.
#'
#' @export
#' @importFrom SpatialExperiment spatialData
#' @family SpatialExperiment-related functions
#' @author Brenda Pardo, Leonardo Collado-Torres
spe_meta <- function(spe) {
    as.data.frame(cbind(
        colData(spe),
        SpatialExperiment::spatialData(spe)
    ))
}
