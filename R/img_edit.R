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
#' @param channel A `character(1)` passed to
#' [magick::image_channel][magick::color]. If `NA` this step is skipped.
#' @param brightness A `numeric(1)` passed to
#' [magick::image_modulate][magick::color].
#' @param saturation A `numeric(1)` passed to
#' [magick::image_modulate][magick::color].
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
#' @param background_color  A `character(1)` passed to
#' [magick::image_background][magick::color]. If `NA` this step is skipped.
#' @param median_radius  A `numeric(1)` passed to
#' [magick::image_median][magick::color]. If `NA` this step is skipped.
#' @param negate A `logical(1)` controlling whether to use
#' [magick::negate][magick::color].
#'
#' @return A `magick` image object such as the one returned by
#' [magick::image_read][magick::editing].
#' @importFrom magick image_read image_modulate image_enhance image_contrast
#' image_quantize image_equalize image_transparent image_median image_negate
#' image_background image_channel
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
#'     x <- img_edit(spe, sampleid = "151507", brightness = 25)
#'     plot(x)
#' }
img_edit <-
    function(spe,
    sampleid,
    image_id = "lowres",
    channel = NA,
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
    background_color = NA,
    median_radius = NA,
    negate = FALSE) {
        img <-
            magick::image_read(SpatialExperiment::imgRaster(spe, sample_id = sampleid, image_id = image_id))


        if (!(is.na(channel) || channel == "")) {
            img <-
                magick::image_channel(img, channel = channel)
        }
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
        if (!is.na(background_color)) {
            img <-
                magick::image_background(img, color = background_color)
        }
        if (!is.na(median_radius)) {
            img <- magick::image_median(img, radius = median_radius)
        }
        if (negate) {
            img <- magick::image_negate(img)
        }

        return(img)
    }
