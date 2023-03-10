asd_sfari <- utils::read.csv(
    system.file(
        "extdata",
        "SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv",
        package = "spatialLIBD"
    ),
    as.is = TRUE
)

## Format them appropriately
asd_sfari_geneList <- list(
    Gene_SFARI_all = asd_sfari$ensembl.id,
    Gene_SFARI_high = asd_sfari$ensembl.id[asd_sfari$gene.score < 3],
    Gene_SFARI_syndromic = asd_sfari$ensembl.id[asd_sfari$syndromic == 1]
)

## Obtain the necessary data
if (!exists("modeling_results")) {
    modeling_results <- fetch_data(type = "modeling_results")
}


## Compute the gene set enrichment results
asd_sfari_enrichment <- gene_set_enrichment(
    gene_list = asd_sfari_geneList,
    modeling_results = modeling_results,
    model_type = "enrichment"
)


test_that("result for each gene list & model test", {
    expect_equal(
        nrow(asd_sfari_enrichment),
        length(asd_sfari_geneList) * length(grep(
            "fdr", colnames(modeling_results$enrichment)
        ))
    )
})

## check behavior for  OR < 1 results
WM_enriched <-
    modeling_results$enrichment$fdr_WM < 0.1 &
        modeling_results$enrichment$t_stat_WM > 0

safari_no_wm_enrich <-
    asd_sfari_geneList$Gene_SFARI_all[asd_sfari_geneList$Gene_SFARI_all %in% modeling_results$enrichment$ensembl[!WM_enriched]]

safari_edge_cases <- list(
    no_WM_enrich = safari_no_wm_enrich,
    short = asd_sfari_geneList$Gene_SFARI_all[1:20]
)

edge_safari_enrichment <- gene_set_enrichment(
    gene_list = safari_edge_cases["no_WM_enrich"],
    modeling_results = modeling_results,
    model_type = "enrichment"
)

## with alternative = "two.sided"
# OR         Pval   test NumSig SetSize           ID model_type fdr_cut
# 1 0.0000000 5.752600e-72     WM      0     638 no_WM_enrich enrichment     0.1

test_that("warn for small gene list", {
    expect_warning(
        gene_set_enrichment(
            gene_list = safari_edge_cases["short"],
            modeling_results = modeling_results,
            model_type = "enrichment"
        )
    )
})

test_that("Not signficant for OR==0", {
    expect_true(all(edge_safari_enrichment$Pval[edge_safari_enrichment$OR == 0] > 0.05))
})
