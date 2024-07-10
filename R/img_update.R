# img_update <- function(spe, sampleid, image_id, new_image_id = paste0("edited_", image_id), brightness = 100, saturation = 100, hue = 100, enhance = FALSE, contrast_sharpen = NA, quantize_max = NA, quantize_dither = TRUE, equalize = FALSE, transparent_color = NA, transparent_fuzz = 0, median_radius = NA, negate = FALSE) {
#     edited_img <- img_edit(spe = spe, sampleid = sampleid, image_id = image_id, brightness = brightness, saturation = saturation, hue = hue, enhance = enhance, contrast_sharpen = contrast_sharpen, quantize_max = quantize_max, quantize_dither = quantize_dither, equalize = equalize, transparent_color = transparent_color, transparent_fuzz = transparent_fuzz, median_radius = median_radius, negate = negate)
#     spi <- SpatialExperiment::SpatialImage(as.raster(edited_img))
#     scaleFactor <- SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id)
#     imgData(spe) <-
#             rbind(
#                 imgData(spe),
#                 DataFrame(
#                     sample_id = sampleid,
#                     image_id = new_image_id,
#                     data = I(list(spi)),
#                     scaleFactor = scaleFactor
#                 )
#             )
#     return(spe)
# }

#' Update the image for one sample
#'
#' Edit the image with `img_edit()` then update the `imgData()`.
#'
#' @inheritParams vis_clus
#' @param new_image_id A `character(1)` specifying the new `image_id` to use.
#' @param overwrite A `logical(1)` specifying whether to overwrite the
#' `image_id` if it already exists.
#' @param ... Parameters passed to `img_edit()`.
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object
#' with an updated `imgData()` slot.
#' @export
#' @importFrom SpatialExperiment imgData "imgData<-" SpatialImage scaleFactors
#' @family Image editing functions
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Reduce brightness to 25% and update the imgData()
#'     imgData(img_update(spe, sampleid = "151507", brightness = 25))
#' }
img_update <-
    function(
        spe,
        sampleid,
        image_id = "lowres",
        new_image_id = paste0("edited_", image_id),
        overwrite = FALSE,
        ...) {
        img_data <- SpatialExperiment::imgData(spe)

        ## Skip this sample if there's no existing image to update
        if (!any(img_data$image_id == image_id & img_data$sample_id == sampleid)) {
            return(spe)
        }

        ## Check if the new image exists
        img_exist_i <- img_data$image_id == new_image_id & img_data$sample_id == sampleid

        if (!overwrite) {
            if (any(img_exist_i)) {
                warning("Did you mean to overwrite image_id '",
                    image_id,
                    "'?",
                    call. = FALSE
                )
                return(spe)
            }
        } else {
            ## Remove the input image to avoid duplicating it later on
            ## when we rbind()
            img_data <- img_data[!img_exist_i, ]
        }

        edited_img <-
            img_edit(
                spe = spe,
                sampleid = sampleid,
                image_id = image_id,
                ...
            )

        spi <-
            SpatialExperiment::SpatialImage(as.raster(edited_img))
        scaleFactor <-
            SpatialExperiment::scaleFactors(spe, sample_id = sampleid, image_id = image_id)

        imgData(spe) <-
            rbind(
                img_data,
                DataFrame(
                    sample_id = sampleid,
                    image_id = new_image_id,
                    data = I(list(spi)),
                    scaleFactor = scaleFactor
                )
            )
        return(spe)
    }
