example("registration_pseudobulk", package = "spatialLIBD")

mod <- registration_model(sce_pseudo)

test_that("Checking for model terms works", {
    expect_equal(sum(grepl("^registration_variable", colnames(mod))), ncol(mod))
    expect_error(registration_model(sce_pseudo, "absent"), "Formula term")
})
