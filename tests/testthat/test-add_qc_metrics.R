test_that(
    "add_qc_metrics returns modified spe",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        # run metrics spe
        spe_qc <- add_qc_metrics(spe)
        expect_equal(ncol(spe), ncol(spe_qc)) ## same number of spots
        expect_equal(ncol(colData(spe)) + 8, ncol(colData(spe_qc))) ## add 8 QC cols to colData
    }
)
