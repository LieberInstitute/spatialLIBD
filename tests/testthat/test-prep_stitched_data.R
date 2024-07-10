test_that(
    "prep_stitched_data",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        #   Missing exclude_overlapping
        expect_error(
            {
                temp <- prep_stitched_data(
                    spe,
                    point_size = 2, image_id = "lowres"
                )
            },
            "^Missing at least one of the following colData"
        )

        #   Can't exclude all spots
        spe$exclude_overlapping <- TRUE
        expect_error(
            {
                temp <- prep_stitched_data(
                    spe,
                    point_size = 2, image_id = "lowres"
                )
            },
            "^spe\\$exclude_overlapping must include some FALSE values to plot$"
        )

        #   Output should be a list with the correct names, and the
        #   SpatialExperiment should have no excluded spots
        spe$exclude_overlapping[1:100] <- FALSE
        temp <- prep_stitched_data(spe, point_size = 2, image_id = "lowres")
        expect_equal(class(temp), "list")
        expect_equal(names(temp), c("spe", "point_size"))
        expect_equal(all(temp$spe$exclude_overlapping), FALSE)

        #   Note bad image_id is not tested, since this function is only used
        #   internally after checks for legitimate image_id are performed
    }
)
