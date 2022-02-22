#' Create a unique spot identifier
#'
#' This function adds `spe$key` to a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object
#' which is unique across all spots.
#'
#' @inheritParams add10xVisiumAnalysis
#' @param overwrite A `logical(1)` indicating whether to overwrite the `spe$key`.
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object
#' with `key` added to the `colData(spe)` that is unique across all spots.
#' @export
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## This object already has a 'key'
#'     head(spe$key)
#'
#'     ## We can clean it
#'     spe$key <- NULL
#'
#'     ## and then add it back
#'     head(add_key(spe)$key)
#'
#'     ## Note that the original 'key' order was 'sample_id'_'barcode' and we'
#'     ## have since changed it to 'barcode'_'sample_id'.
#' }
add_key <- function(spe, overwrite = TRUE) {
    if ("key" %in% colnames(colData(spe))) {
        if (overwrite) {
            message(
                "Overwriting 'spe$key'. Set 'overwrite = FALSE' if you do not want to overwrite it."
            )
        } else {
            stop(
                "'spe$key' already exists. Set 'overwrite = TRUE' if you want to replace it.",
                call. = FALSE
            )
        }
    }
    spe$key <- paste0(colnames(spe), "_", spe$sample_id)
    stopifnot(!any(duplicated(spe$key)))
    return(spe)
}
