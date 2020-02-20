## code to prepare `tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer` dataset goes here

load(
    here::here(
        'data-raw',
        'tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.Rdata'
    ),
    verbose = TRUE
)
head(tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer)

write.csv(
    tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
    file = here::here(
        'data-raw',
        'tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv'
    ),
    quote = FALSE,
    row.names = TRUE
)

test <- read.csv(
    here::here(
        'data-raw',
        'tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv'
    ),
    header = TRUE,
    stringsAsFactors = FALSE,
    na.strings = "",
    check.names = FALSE,
    row.names = 1
)
testthat::expect_equivalent(tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
    as.matrix(test))

usethis::use_data(tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer, overwrite = TRUE)
