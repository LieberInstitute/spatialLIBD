#' Add analysis data from a 10x Genomics Visium experiment to a SPE object
#'
#' This function adds to a SPE
#' ([SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class])
#' object the output from `read10xVisiumAnalysis()`.
#'
#' You might want to use `read10xVisiumWrapper()` instead of using this
#' function directly.
#'
#' @param spe A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object.
#' @param visium_analysis The output from `read10xVisiumAnalysis()`.
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object
#' with the clustering results from SpaceRanger added to `colData(spe)` and
#' the dimension reduction results added to `reducedDims(spe)`. Added data
#' starts with the `10x_` prefix to make them easy to differentiate.
#' @export
#' @family Utility functions for reading data from SpaceRanger output by 10x
#' Genomics
#'
#' @examples
#' ## See 'Using spatialLIBD with 10x Genomics public datasets' for
#' ## a full example using this function.
#' if (interactive()) {
#'     browseVignettes(package = "spatialLIBD")
#' }
#'
#' ## Note that ?SpatialExperiment::read10xVisium doesn't include all the files
#' ## we need to illustrate read10xVisiumWrapper().
add10xVisiumAnalysis <- function(spe,
    visium_analysis) {
    col_info <- colData(spe)
    barcode_present <- "barcode" %in% colnames(col_info)
    if (!barcode_present) {
        col_info$barcode <- rownames(col_info)
    }

    merged_info <-
        merge(
            col_info,
            visium_analysis$clusters,
            by = c("barcode", "sample_id"),
            sort = FALSE,
            all = TRUE
        )

    spe <- add_key(spe)
    key_analysis <- paste0(merged_info$barcode, "_", merged_info$sample_id)
    merged_info <- merged_info[match(spe$key, key_analysis), , drop = FALSE]

    if (!barcode_present) {
        merged_info$barcode <- NULL
    }

    basic_structure <- col_info[, c("barcode", "sample_id")]
    projections_list <- lapply(visium_analysis$projections, function(x) {
        merged_projection <- merge(basic_structure, x, sort = FALSE, all = TRUE)
        key_projection <- paste0(merged_projection$barcode, "_", merged_projection$sample_id)
        m_proj <- match(spe$key, key_projection)
        result <- as.matrix(merged_projection[m_proj, -which(colnames(merged_projection) %in% c("barcode", "sample_id")), drop = FALSE])
        rownames(result) <- rownames(col_info)
        return(result)
    })

    reducedDims(spe) <- c(reducedDims(spe), projections_list)
    colData(spe) <- DataFrame(merged_info, check.names = FALSE)
    colnames(spe) <- rownames(col_info)

    return(spe)
}
