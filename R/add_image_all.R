add_image_all <-
    function(spe,
        image_dir,
        image_pattern,
        image_id_current = "lowres",
        image_id = image_pattern,
        overwrite = FALSE) {
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
            spe <- add_image(
                spe = spe,
                image_path = image_path,
                sample_id = sample_id,
                scale_factor = scale_factor,
                image_id = image_id,
                overwrite = overwrite
            )

        }
        return(spe)
    }
