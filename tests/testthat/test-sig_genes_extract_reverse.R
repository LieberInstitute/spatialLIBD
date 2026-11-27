test_that("sig_genes_extract() negates logFC when reverse = TRUE", {
    sce <- SummarizedExperiment::SummarizedExperiment(
        assays = list(logcounts = matrix(0, nrow = 6, ncol = 4)),
        rowData = S4Vectors::DataFrame(gene_name = paste0("G", seq_len(6)))
    )
    rownames(sce) <- paste0("ENSG000", seq_len(6))

    modeling_results <- list(
        pairwise = data.frame(
            `t_stat_Layer1-Layer2` = c(10, 5, 1, -1, -5, -10),
            `p_value_Layer1-Layer2` = c(1e-6, 1e-3, 0.3, 0.3, 1e-3, 1e-6),
            `fdr_Layer1-Layer2` = c(3e-6, 1.5e-3, 0.3, 0.3, 1.5e-3, 3e-6),
            `logFC_Layer1-Layer2` = c(3.0, 1.5, 0.2, -0.2, -1.5, -3.0),
            ensembl = rownames(sce),
            gene = paste0("G", seq_len(6)),
            check.names = FALSE
        ),
        enrichment = data.frame(
            t_stat_Layer1 = c(8, 4, 1, -1, -4, -8),
            p_value_Layer1 = c(1e-5, 1e-3, 0.4, 0.4, 1e-3, 1e-5),
            fdr_Layer1 = c(3e-5, 1.5e-3, 0.4, 0.4, 1.5e-3, 3e-5),
            logFC_Layer1 = c(2.5, 1.2, 0.1, -0.1, -1.2, -2.5),
            ensembl = rownames(sce),
            gene = paste0("G", seq_len(6)),
            check.names = FALSE
        )
    )

    pw_fwd <- sig_genes_extract(
        n = 2,
        modeling_results = modeling_results,
        model_type = "pairwise",
        reverse = FALSE,
        sce_layer = sce
    )
    expect_equal(pw_fwd$test, c("Layer1-Layer2", "Layer1-Layer2"))
    expect_equal(pw_fwd$stat, c(10, 5))
    expect_equal(pw_fwd$logFC, c(3.0, 1.5))

    pw_rev <- sig_genes_extract(
        n = 2,
        modeling_results = modeling_results,
        model_type = "pairwise",
        reverse = TRUE,
        sce_layer = sce
    )
    expect_equal(pw_rev$test, c("Layer2-Layer1", "Layer2-Layer1"))
    expect_equal(pw_rev$gene, c("G6", "G5"))
    expect_equal(pw_rev$stat, c(10, 5))
    expect_equal(pw_rev$logFC, c(3.0, 1.5))

    enr_rev <- sig_genes_extract(
        n = 2,
        modeling_results = modeling_results,
        model_type = "enrichment",
        reverse = TRUE,
        sce_layer = sce
    )
    expect_equal(enr_rev$gene, c("G6", "G5"))
    expect_equal(enr_rev$stat, c(8, 4))
    expect_equal(enr_rev$logFC, c(2.5, 1.2))
})
