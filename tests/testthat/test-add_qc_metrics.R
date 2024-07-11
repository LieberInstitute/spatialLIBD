test_that(
    "add_qc_metrics returns modified spe",
    {
        if (!exists("spe")) spe <- fetch_data("spe")

        # run metrics spe
        spe_qc <- add_qc_metrics(spe, overwrite = TRUE)
        expect_equal(ncol(spe), ncol(spe_qc)) ## same number of spots
        expect_equal(ncol(colData(spe)) + 7, ncol(colData(spe_qc))) ## add 7 QC cols to colData
        # [1] "scran_discard"                   "scran_low_lib_size"              "scran_low_n_features"
        # [4] "scran_high_subsets_Mito_percent" "edge_spot"                       "edge_distance"
        # [7] "scran_low_lib_size_edge"
        rm(spe_qc)
    }
)
