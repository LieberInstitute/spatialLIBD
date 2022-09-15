example("registration_pseudobulk", package = "spatialLIBD")

sce$batches <- sample(1:3, ncol(sce), replace = TRUE)
test_that("NA check works", {
    expect_error(
        registration_pseudobulk(sce, "Treatment", "sample_id", c("age", "batches")),
        "has all NAs after pseudo-bulking"
    )
    expect_error(
        registration_pseudobulk(sce, "CellCyle", "sample_id", c("age", "Treatment")),
        "var_registration"
    )
})
