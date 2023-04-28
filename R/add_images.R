#' Add non-standard images with the same dimensions as current ones
#'
#' This function re-uses the `SpatialExperiment::scaleFactors()` from current
#' images when adding new images. This is useful if you take for example a
#' multi-channel VisiumIF image and break into several single-channel images
#' that all have the same dimensions. So you could have a set of images such as
#' `channel_01_lowres` and `channel_02_lowres` that have the same dimensions
#' and viewing area as the `lowres` image produced by SpaceRanger, each with
#' only one channel. Similarly, you might have done some image manipulation for
#' a given image and generated one or more images with the same dimensions as
#' existing images.
#'
#' @inheritParams locate_images
#' @param image_id_current A `character(1)` specifying the name of the current
#' existing image in `spe` that has the same scaling factor that to be used
#' with the additional images.
#' @param image_id A `character(1)` specifying the name to use in the new
#' images. It cannot be the same as one used for existing images in `spe` for a
#' given sample. It equals `image_pattern` by default.
#' @param image_paths A named `character()` vector with the paths to the images.
#' The names have to match the `spe$sample_id` and cannot be repeated. By
#' default `locate_images()` is used but you can alternatively specify
#' `image_paths` and ignore `image_dir` and `image_pattern`.
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object
#' with the additional image data in `imgData(spe)`.
#' @importFrom SpatialExperiment addImg scaleFactors
#' @export
#' @family Functions for adding non-standard images
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Add an image
#'     SpatialExperiment::imgData(add_images(
#'         spe,
#'         image_id_current = "lowres",
#'         image_id = "lowres_aws",
#'         image_paths = c("151507" = "https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_lowres_image.png")
#'     ))
#' }
add_images <-
    function(
        spe,
        image_dir,
        image_pattern,
        image_id_current = "lowres",
        image_id = image_pattern,
        image_paths = locate_images(spe, image_dir, image_pattern)) {
        stopifnot(length(names(image_paths)) > 0)
        stopifnot(all(names(image_paths) %in% unique(spe$sample_id)))
        stopifnot(!any(duplicated(names(image_paths))))
        for (sample_id in unique(spe$sample_id)) {
            message(Sys.time(), " adding image for sample ", sample_id)
            image_path <- image_paths[sample_id]
            if (length(image_path[!is.na(image_path)]) == 0) {
                warning("No image was found for sample: ", sample_id, call. = FALSE)
                next
            }
            scale_factor <-
                SpatialExperiment::scaleFactors(spe, sample_id = sample_id, image_id = image_id_current)
            spe <- SpatialExperiment::addImg(
                spe,
                imageSource = image_path,
                scaleFactor = scale_factor,
                sample_id = sample_id,
                image_id = image_id,
                load = TRUE
            )
        }
        return(spe)
    }
