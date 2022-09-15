example("registration_pseudobulk", package = "spatialLIBD")

mod <- registration_model(sce_pseudo)


test_that("Checking for model terms works", {
    expect_equal(sum(grepl(
        "^registration_variable", colnames(mod)
    )), ncol(mod))
    expect_error(registration_model(sce_pseudo, "absent"), "Formula term")

    sce_pseudo_empty_levels <- sce_pseudo
    sce_pseudo_empty_levels$registration_variable <-
        factor(
            sce_pseudo_empty_levels$registration_variable,
            levels = c(
                unique(sce_pseudo_empty_levels$registration_variable),
                "emptyLevel"
            )
        )
    expect_error(
        registration_model(sce_pseudo_empty_levels),
        "resulting model is not full rank"
    )
})
