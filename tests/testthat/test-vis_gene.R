test_that(
    "vis_gene",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        #   Non-numeric column to plot
        expect_error(
            {
                p <- vis_gene(
                    spe,
                    geneid = c("sum_umi", rownames(spe)[1], "layer_guess")
                )
            },
            "'geneid' can not contain non-numeric colData columns\\."
        )

        #   Bad sample ID
        expect_error(
            {
                p <- vis_gene(
                    spe,
                    geneid = c("sum_umi", rownames(spe)[1]),
                    sampleid = "aaa"
                )
            },
            "'spe\\$sample_id' must exist and contain the ID aaa"
        )

        #   Bad assayname
        expect_error(
            {
                p <- vis_gene(
                    spe,
                    geneid = c("sum_umi", rownames(spe)[1]),
                    assayname = "aaa"
                )
            },
            "'aaa' is not an assay in 'spe'"
        )

        #   Bad geneid
        expect_error(
            {
                p <- vis_gene(spe, geneid = "aaa")
            },
            "Could not find the 'geneid'\\(s\\) aaa"
        )

        #   Missing exclude_overlapping
        expect_error(
            {
                p <- vis_gene(spe, geneid = "sum_umi", is_stitched = TRUE)
            },
            "^Missing at least one of the following colData"
        )

        #   Can't exclude all spots
        spe$exclude_overlapping = TRUE
        expect_error(
            {
                p <- vis_gene(spe, geneid = "sum_umi", is_stitched = TRUE)
            },
            "^spe\\$exclude_overlapping must include some FALSE values to plot$"
        )

        #   Trivially check success with legitimate input
        expect_equal(
            class(vis_gene(spe, geneid = c("sum_umi", rownames(spe)[1]))),
            c("gg", "ggplot")
        )


        #   Bad spatialCoords
        colnames(spatialCoords(spe)) = c('a', 'b')
        expect_error(
            { p <- vis_gene(spe, geneid = "sum_umi") },
            "^Abnormal spatial coordinates"
        )
    }
)
