#' Update the images for all samples
#'
#' This function uses `img_update()` for all samples. That is, it loops through
#' every sample and edits the image with `img_edit()` and then updates the
#' `imgData()`.
#'
#' @inheritParams img_update
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object
#' with an updated `imgData()` slot.
#' @export
#' @family Image editing functions
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Reduce brightness to 25% for the 'lowres' image for all samples and
#'     ## update the imgData()
#'     imgData(img_update_all(spe, brightness = 25))
#' }
img_update_all <-
    function(
        spe,
        image_id = "lowres",
        new_image_id = paste0("edited_", image_id),
        overwrite = FALSE,
        ...) {
        for (sampleid in unique(spe$sample_id)) {
            spe <-
                img_update(
                    spe = spe,
                    sampleid = sampleid,
                    image_id = image_id,
                    new_image_id = new_image_id,
                    overwrite = overwrite,
                    ...
                )
        }
        return(spe)
    }
