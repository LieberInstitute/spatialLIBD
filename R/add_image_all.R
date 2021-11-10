#' Title
#'
#' @param spe
#' @param image_dir
#' @param image_pattern
#' @param image_id_current
#' @param image_id
#'
#' @return
#' @importFrom SpatialExperiment addImg scaleFactors
#' @export
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Add an image
#'     SpatialExperiment::imgData(addImg(
#'         spe,
#'         imageSource = "https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_hires_image.png",
#'         sample_id = "151507",
#'         image_id = "hires",
#'         scaleFactor = 0.150015
#'     ))
#'
#'     ## scale factor information from
#'     ## https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151507/scalefactors_json.json
#' }
add_image_all <-
    function(spe,
        image_dir,
        image_pattern,
        image_id_current = "lowres",
        image_id = image_pattern) {
        for (sample_id in unique(spe$sample_id)) {
            message(Sys.time(), " adding image for sample ", sample_id)
            image_path <-
                file.path(image_dir,
                    paste0(sample_id, "_", image_pattern, ".png"))
            if (!file.exists(image_path)) {
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
