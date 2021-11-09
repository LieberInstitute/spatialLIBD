#' Edit a background image
#'
#' This function uses the `magick` package to edit the color and perform other
#' image manipulations on a background image. It can be useful if you want to
#' highlight certain features of these images.
#'
#' The `magick` functions are used in the sequence represented by the arguments
#' to this function. You can alternatively use this function sequentially. Or
#' directly use the `magick` package.
#'
#' @inheritParams vis_clus
#' @param brightness A `numeric(1)` passed to [magick::image_modulate][magick::color].
#' @param saturation A `numeric(1)` passed to [magick::image_modulate][magick::color].
#' @param hue A `numeric(1)` passed to [magick::image_modulate][magick::color].
#' @param enhance A `logical(1)` controlling whether to use
#' [magick::enhance][magick::color].
#' @param normalize A `logical(1)` controlling whether to use
#' [magick::normalize][magick::color].
#' @param contrast_sharpen A `numeric(1)` passed to
#' [magick::image_contrast][magick::color]. If `NA` this step is skipped.
#' @param quantize_max A `numeric(1)` passed to
#' [magick::image_quantize][magick::color]. If `NA` this step is skipped.
#' @param quantize_dither A `logical(1)` passed to
#' [magick::image_quantize][magick::color].
#' @param equalize A `logical(1)` controlling whether to use
#' [magick::equalize][magick::color].
#' @param transparent_color  A `character(1)` passed to
#' [magick::image_transparent][magick::color]. If `NA` this step is skipped.
#' @param transparent_fuzz A `numeric(1)` passed to
#' [magick::image_transparent][magick::color].
#' @param median_radius  A `numeric(1)` passed to
#' [magick::image_median][magick::color]. If `NA` this step is skipped.
#' @param negate A `logical(1)` controlling whether to use
#' [magick::negate][magick::color].
#'
#' @return
#' @importFrom magick image_read image_modulate image_enhance image_contrast
#' image_quantize image_equalize image_transparent image_median image_negate
#' @importFrom SpatialExperiment imgRaster
#' @export
#' @family Image editing functions
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Reduce brightness to 25%
#'     img_edit(spe, sampleid = "151507", brightness = 25)
#' }
img_edit <-
    function(spe,
    sampleid,
    image_id = "lowres",
    brightness = 100,
    saturation = 100,
    hue = 100,
    enhance = FALSE,
    contrast_sharpen = NA,
    quantize_max = NA,
    quantize_dither = TRUE,
    equalize = FALSE,
    normalize = FALSE,
    transparent_color = NA,
    transparent_fuzz = 0,
    median_radius = NA,
    negate = FALSE) {
        img <-
            magick::image_read(SpatialExperiment::imgRaster(spe, sample_id = sampleid, image_id = image_id))

        img <-
            magick::image_modulate(img,
                brightness = brightness,
                saturation = saturation,
                hue = hue
            )
        if (enhance) {
              img <- magick::image_enhance(img)
          }
        if (!is.na(contrast_sharpen)) {
              img <-
                  magick::image_contrast(img, sharpen = contrast_sharpen)
          }
        if (!is.na(quantize_max)) {
              img <-
                  magick::image_quantize(img, max = quantize_max, dither = quantize_dither)
          }
        if (equalize) {
              img <- magick::image_equalize(img)
          }
        if (normalize) {
              img <- magick::image_normalize(img)
          }
        if (!is.na(transparent_color)) {
              img <-
                  magick::image_transparent(img, color = transparent_color, fuzz = transparent_fuzz)
          }
        if (!is.na(median_radius)) {
              img <- magick::image_median(img, radius = median_radius)
          }
        if (negate) {
              img <- magick::image_negate(img)
          }

        return(img)
    }

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
    function(spe,
    sampleid,
    image_id = "lowres",
    new_image_id = paste0("edited_", image_id),
    overwrite = FALSE,
    ...) {
        img_data <- SpatialExperiment::imgData(spe)
        if (!overwrite) {
            if (image_id == new_image_id) {
                warning("Did you mean to overwrite image_id '",
                    image_id,
                    "'?",
                    call. = FALSE
                )
                return(spe)
            }
        } else {
            img_data <-
                subset(
                    img_data,
                    !(image_id == new_image_id & sample_id == sampleid)
                )
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
    function(spe,
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
