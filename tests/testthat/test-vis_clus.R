test_that(
    "vis_clus",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        #   Bad spatialCoords
        spe_temp <- spe
        colnames(spatialCoords(spe_temp)) <- c("a", "b")
        expect_error(
            {
                p <- vis_clus(spe_temp, clustervar = "sample_id")
            },
            "^Abnormal spatial coordinates"
        )
        rm(spe_temp)
    }
)
