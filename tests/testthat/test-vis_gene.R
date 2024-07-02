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

        #   Trivially check success with legitimate input
        expect_equal(
            class(vis_gene(spe, geneid = c("sum_umi", rownames(spe)[1]))),
            c("gg", "ggplot")
        )


        #   Bad spatialCoords
        spe_temp <- spe
        colnames(spatialCoords(spe_temp)) <- c("a", "b")
        expect_error(
            {
                p <- vis_gene(spe_temp, geneid = "sum_umi")
            },
            "^Abnormal spatial coordinates"
        )
        rm(spe_temp)
    }
)
