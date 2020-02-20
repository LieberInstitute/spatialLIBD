asd_sfari <- utils::read.csv(
    system.file(
        'extdata',
        'SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv',
        package = 'spatialLIBD'
    ),
    as.is = TRUE
)
asd_sfari_geneList <- list(
    Gene_SFARI_all = asd_sfari$ensembl.id,
    Gene_SFARI_high = asd_sfari$ensembl.id[asd_sfari$gene.score < 3],
    Gene_SFARI_syndromic = asd_sfari$ensembl.id[asd_sfari$syndromic == 1]
)
lengths(asd_sfari_geneList)
# Gene_SFARI_all      Gene_SFARI_high Gene_SFARI_syndromic
#            913                  441                  119

## It doesn't matter that some are NAs as
## gene_set_enrichment() will filter them out
head(asd_sfari_geneList[[2]])
# [1] "ENSG00000087085" "ENSG00000075624" NA                NA                "ENSG00000196839" "ENSG00000138031"

## Make into a rectangular format
m_length <- max(lengths(asd_sfari_geneList))
asd_sfari_geneList_table <-
    do.call(cbind, lapply(asd_sfari_geneList, function(x) {
        if (length(x) < m_length) {
            res <- rep(NA, m_length)
            res[seq_len(length(x))] <- x
        } else {
            res <- x
        }
        return(res)
    }))

write.csv(
    asd_sfari_geneList_table,
    file = here::here('data-raw', 'asd_sfari_geneList.csv'),
    row.names = FALSE,
    quote = FALSE
)


## Test that re-reading the file works
test <- read.csv(
    here::here('data-raw', 'asd_sfari_geneList.csv'),
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
)
testthat::expect_equivalent(asd_sfari_geneList_table, as.matrix(test))


## Test that the input data works for gene_set_enrichment()
ori_modeling_results <- fetch_data(type = 'modeling_results')

asd_sfari_enrichment <- gene_set_enrichment(
    gene_list = asd_sfari_geneList,
    modeling_results = ori_modeling_results,
    model_type = 'specificity'
)
test_enrichment <- gene_set_enrichment(
    gene_list = test,
    modeling_results = ori_modeling_results,
    model_type = 'specificity'
)

stopifnot(identical(asd_sfari_enrichment, test_enrichment))
