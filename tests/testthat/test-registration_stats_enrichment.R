example("registration_block_cor", package = "spatialLIBD")

results_enrichment <- registration_stats_enrichment(sce_pseudo,
    block_cor,
    "age",
    gene_ensembl = "ensembl",
    gene_name = "gene_name"
)
results_enrichment_nan <- registration_stats_enrichment(
    sce_pseudo,
    block_cor = NaN,
    "age",
    gene_ensembl = "ensembl",
    gene_name = "gene_name"
)

test_that("NaN works", {
    expect_equal(dim(results_enrichment), dim(results_enrichment_nan))

    expect_equal(
        mean(
            results_enrichment$t_stat_G0 - results_enrichment_nan$t_stat_G0
        ),
        -0.0001541423
    )
})
