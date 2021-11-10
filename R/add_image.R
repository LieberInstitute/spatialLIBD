add_image <-
    function(spe,
        image_path,
        sample_id,
        scale_factor,
        image_id = gsub("\\..*", "", basename(image_path)),
        overwrite = FALSE) {
        if (!overwrite) {
            if (length(
                SpatialExperiment::scaleFactors(spe, sample_id = sample_id, image_id = image_id)
            ) > 0) {
                warning("Did you mean to overwrite image_id '",
                    image_id,
                    "'?",
                    call. = FALSE)
                return(spe)
            }
        } else {
            ## Remove the input image to avoid duplicating it later on
            ## when we rbind()
            img_data <- SpatialExperiment::imgData(spe)
            img_data <- img_data[ !(img_data$image_id == image_id & img_data$sample_id == sample_id),  ]
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
