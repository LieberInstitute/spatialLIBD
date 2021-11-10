#' Title
#'
#' @param spe
#' @param image_path
#' @param sample_id A `character(1)`
#' @param image_id A `character(1)` specifying the ID to use for the new image.
#' @param scale_factor A `numeric(1)` with the scaling factor. If you are using
#' an image that has the same dimensions as an existing one, you could use
#' [SpatialExperiment::scaleFactors][SpatialExperiment::SpatialExperiment-methods].
#' @param overwrite
#'
#' @return
#' @importFrom SpatialExperiment imgData "imgData<-" scaleFactors
#' @export
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Add an image
#'     SpatialExperiment::imgData(add_image(
#'         spe = spe,
#'         image_path = "https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_hires_image.png",
#'         sample_id = "151507",
#'         image_id = "hires",
#'         scale_factor = 0.150015
#'     ))
#'
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
add_image <-
    function(spe,
        image_path,
        sample_id,
        image_id = gsub("\\..*", "", basename(image_path)),
        scale_factor = SpatialExperiment::scaleFactors(spe, sample_id = sample_id, image_id = image_id),
        overwrite = FALSE) {
        img_data <- SpatialExperiment::imgData(spe)
        img_exist_i <- img_data$image_id == image_id & img_data$sample_id == sample_id

        if (!overwrite) {
            if (any(img_exist_i)) {
                warning("Did you mean to overwrite image_id '",
                    image_id,
                    "'?",
                    call. = FALSE)
                return(spe)
            }
        } else {
            ## Remove the input image to avoid duplicating it later on
            ## when we rbind()
            img_data <- img_data[ !(img_exist_i),  ]
        }

        new_image_data <-
            SpatialExperiment:::.get_imgData(
                img = image_path,
                scaleFactor = scale_factor,
                sample_id = sample_id,
                image_id = image_id,
                load = TRUE
            )

        imgData(spe) <-
            rbind(img_data,
                new_image_data)
        return(spe)
    }
