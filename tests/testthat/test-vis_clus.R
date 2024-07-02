test_that(
    "vis_clus",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        #   Bad spatialCoords
        colnames(spatialCoords(spe)) = c('a', 'b')
        expect_error(
            { p <- vis_clus(spe, clustervar = "sample_id") },
            "^Abnormal spatial coordinates"
        )
    }
)
