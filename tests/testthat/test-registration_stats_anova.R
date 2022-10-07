## Ensure reproducibility of example data
set.seed(20220907)

## Generate example data
sce <- scuttle::mockSCE()

## Add some sample IDs
sce$sample_id <- sample(LETTERS[1:5], ncol(sce), replace = TRUE)

## Add a sample-level covariate: age
ages <- rnorm(5, mean = 20, sd = 4)
names(ages) <- LETTERS[1:5]
sce$age <- ages[sce$sample_id]

## Add gene-level information
rowData(sce)$ensembl <- paste0("ENSG", seq_len(nrow(sce)))
rowData(sce)$gene_name <- paste0("gene", seq_len(nrow(sce)))

## Pseudo-bulk
sce_pseudo <-
    registration_pseudobulk(sce, "Treatment", "sample_id", c("age"))

registration_mod <- registration_model(sce_pseudo, "age")
block_cor <- registration_block_cor(sce_pseudo, registration_mod)

test_that("Check that we have F-statistics", {
    expect_error(
        registration_stats_anova(
            sce_pseudo,
            block_cor,
            "age",
            gene_ensembl = "ensembl",
            gene_name = "gene_name",
            suffix = "example"
        ),
        "at least 3 different values"
    )
})
