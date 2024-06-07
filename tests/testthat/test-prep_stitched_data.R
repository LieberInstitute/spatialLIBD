test_that(
    "prep_stitched_data",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        #   Missing exclude_overlapping
        expect_error(
            {
                temp <- prep_stitched_data(
                    spe, point_size = 2, image_id = "lowres"
                )
            },
            "^Missing at least one of the following colData"
        )

        #   Can't exclude all spots
        spe$exclude_overlapping = TRUE
        expect_error(
            {
                temp <- prep_stitched_data(
                    spe, point_size = 2, image_id = "lowres"
                )
            },
            "^spe\\$exclude_overlapping must include some FALSE values to plot$"
        )

        #   Note bad image_id is not tested, since this function is only used
        #   internally after checks for legitimate image_id are performed
    }
)
