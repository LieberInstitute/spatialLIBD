test_that(
    "metrics_qc returns modified spe",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        # run metrics spe
        spe_qc <- metrics_qc(spe)
        expect_equal(ncol(spe), ncol(spe_qc)) ## same number of spots
        expect_equal(ncol(colData(spe)) + 8, ncol(colData(spe_qc))) ## add 8 QC cols to colData
    }
)
