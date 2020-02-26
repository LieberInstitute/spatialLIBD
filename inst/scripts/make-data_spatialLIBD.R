## Run this on JHPCE at
## /dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/spatialLIBD_files

library('SingleCellExperiment')
library('here')
library('sessioninfo')

## Output directory
dir.create(here('Analysis',
    'spatialLIBD_files'), showWarnings = FALSE)

## Load data
load(here(
    'Analysis',
    'Human_DLPFC_Visium_processedData_sce_scran.Rdata'
),
    verbose = TRUE)

load(here('Analysis',
    'rda_scran',
    'clust_k5_list.Rdata'),
    verbose = TRUE)
load(
    here(
        'Analysis',
        'rda_scran',
        'clust_10x_layer_maynard_martinowich.Rdata'
    ),
    verbose = TRUE
)

load(here('Analysis', 'Layer_Guesses', 'rda',
    'layer_guess_tab.Rdata'),
    verbose = TRUE)


## From the original spatialLIBD code
clust_k5 <- do.call(cbind, clust_k5_list)
colnames(clust_k5) <- paste0('SNN_k50_', colnames(clust_k5))
rownames(clust_k5) <- NULL
colData(sce) <- cbind(colData(sce), clust_k5)

sce$Cluster10X <- sce$Cluster
sce$Maynard <- clust_10x_layer_maynard
sce$Martinowich <- clust_10x_layer_martinowich

sce$Layer <- "NA"


## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity.R
sce$layer_guess <- NA
m <- match(sce$key, layer_guess_tab$key)
sce$layer_guess[!is.na(m)] <- layer_guess_tab$layer[m[!is.na(m)]]
sce$layer_guess[which(sce$layer_guess == 'Layer 2/3')] <- 'Layer 3'
sce$layer_guess <-
    factor(gsub(' ', '', sce$layer_guess), levels = c('WM', paste0('Layer', seq_len(6))))
sce$layer_guess_reordered <-
    factor(sce$layer_guess, levels = c(paste0('Layer', seq_len(6)), 'WM'))
sce$layer_guess_reordered_short <- sce$layer_guess_reordered
levels(sce$layer_guess_reordered_short) <-
    gsub('ayer', '', levels(sce$layer_guess_reordered))

## From https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/misc_numbers.R
ix_mito <- grep("^MT-", rowData(sce)$gene_name)
sce$expr_chrM <- colSums(assays(sce)$counts[ix_mito,])
sce$expr_chrM_ratio <- sce$expr_chrM / sce$sum_umi
## Manually compare vs the info from the other script
summary(sce$expr_chrM_ratio)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.04853 0.15465 0.18442 0.18554 0.21521 0.44156

## Fix the rowRanges
## From https://github.com/LieberInstitute/HumanPilot/blob/c8a3a31b991081d656ededee59da45aa0494b334/Analysis/Layer_Notebook.R#L78-L87
map = read.delim(
    here(
        "/10X/151675/151675_raw_feature_bc_matrix__features.tsv.gz"
    ),
    as.is = TRUE,
    header = FALSE,
    col.names = c("EnsemblID", "Symbol", "Type")
)
## get GTF, this seems like what they used
gtf = rtracklayer::import(
    "/dcl01/ajaffe/data/lab/singleCell/refdata-cellranger-GRCh38-3.0.0/genes/genes.gtf"
)
gtf = gtf[gtf$type	== "gene"]
names(gtf) = gtf$gene_id
gtf = gtf[map$EnsemblID]
seqlevels(gtf)[seq_len(25)] = paste0("chr", seqlevels(gtf)[seq_len(25)])
# mcols(gtf) = mcols(gtf)[,c(5:9)]

## Keep the non-empty mcols()
mcols_empty_n <- sapply(mcols(gtf), function(x)
    sum(is.na(x)))
names(mcols_empty_n[mcols_empty_n == 0])
# [1] "source"       "type"         "gene_id"      "gene_version" "gene_name"
# [6] "gene_source"  "gene_biotype"
mcols(gtf) <- mcols(gtf)[, names(mcols_empty_n[mcols_empty_n == 0])]

## Check the order
stopifnot(identical(rownames(sce), names(gtf)))

## Replace the rowRanges info
rowRanges(sce) <- gtf

## To simplify other code later
rowData(sce)$gene_search <-
    paste0(rowData(sce)$gene_name, '; ', rowData(sce)$gene_id)

## Double check our selection of chrM genes now that we have the
## correct rowRanges data
stopifnot(identical(which(seqnames(sce) == 'chrMT'), ix_mito))

## Add whether the gene is a top highly variable gene (HVG) or not
rowData(sce)$is_top_hvg <- rownames(sce) %in% top.hvgs


## Add more cluster results?
## Likely the ones from Lukas
## initial k-means ones from 10x?

save(
    sce,
    file = here(
        'Analysis',
        'spatialLIBD_files',
        'Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata'
    )
)







## Now for the sce_layer object
## note that this re-loads the top.hvgs object
load(here('Analysis', 'Layer_Guesses', 'rda', 'sce_layer.Rdata'),
    verbose = TRUE)

## Fix the rowRanges info
rowRanges(sce_layer) <-
    rowRanges(sce)[match(rownames(sce_layer), rownames(sce)), ]

## Save the top HVG (layer-level) info
rowData(sce_layer)$is_top_hvg_sce_layer <-
    rownames(sce_layer) %in% top.hvgs

## For the different plots
sce_layer$layer_guess_reordered <-
    factor(sce_layer$layer_guess, levels = c(paste0('Layer', seq_len(6)), 'WM'))
sce_layer$layer_guess_reordered_short <-
    sce_layer$layer_guess_reordered
levels(sce_layer$layer_guess_reordered_short) <-
    gsub('ayer', '', levels(sce_layer$layer_guess_reordered))


save(
    sce_layer,
    file = here(
        'Analysis',
        'spatialLIBD_files',
        'Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata'
    )
)


## Also the modeling results
load(here('Analysis',
    'Layer_Guesses',
    'rda',
    'modeling_results.Rdata'),
    verbose = TRUE)

modeling_results <- list('anova' = results_anova,
    'enrichment' = results_specificity,
    'pairwise' = results_pairwise)

save(
    modeling_results,
    file = here(
        'Analysis',
        'spatialLIBD_files',
        'Human_DLPFC_Visium_modeling_results.Rdata'
    )
)

## List all files
dir(here('Analysis',
    'spatialLIBD_files'))
# [1] "Human_DLPFC_Visium_modeling_results.Rdata"
# [2] "Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata"
# [3] "Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata"

######################################
##### Run locally (not on JHPCE) #####
######################################

usethis::use_directory("data-raw/spatialLIBD_files", ignore = TRUE)
system(
    paste0(
        'scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/spatialLIBD_files/* ',
        here::here('data-raw/spatialLIBD_files'),
        '/'
    )
)

## Update on 2020-02-21 the modeling results only
# system(
#     paste0(
#         'scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/spatialLIBD_files/Human_DLPFC_Visium_modeling_results.Rdata ',
#         here::here('data-raw/spatialLIBD_files'),
#         '/'
#     )
# )

system(paste(
    'echo data-raw/spatialLIBD_files >>',
    here::here('.gitignore')
))

## Add the clusters from Lukas M Weber and Stephanie Hicks
library('SingleCellExperiment')
load(
    here::here(
        'data-raw',
        'Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata'
    ),
    verbose = TRUE
)

local_cluster_csvs <-
    dir(
        '~/Dropbox/code/HumanPilot/outputs/SpatialDE_clustering/',
        pattern = '^cluster_labels_.*.csv',
        full.names = TRUE
    )
csv_file <- local_cluster_csvs[1]
spatial_clus <-
    do.call(rbind,
        lapply(local_cluster_csvs, read.csv, stringsAsFactors = FALSE))

## It's too big to include as data in the pkg
pryr::object_size(spatial_clus)
# 8.01 MB

## Check the order
stopifnot(identical(spatial_clus$key, sce$key))

## Drop the 'key' and 'ground_truth' since we don't need it
spatial_clus <-
    spatial_clus[, -which(colnames(spatial_clus) %in% c('key', 'ground_truth'))]

## Append
colData(sce) <- cbind(colData(sce), spatial_clus)

## Rename Cluster10X to something else
colnames(colData(sce))[colnames(colData(sce)) == 'Cluster10X'] <-
    'GraphBased'

## Double check that it all works!
# ori_sce_layer <-
#     fetch_data('sce_layer', here::here('data-raw/spatialLIBD_files'))
# ori_modeling_results <-
#     fetch_data('modeling_results',
#         here::here('data-raw/spatialLIBD_files'))
#
# ori_sig_genes <-
#     sig_genes_extract_all(n = nrow(ori_sce_layer),
#         ori_modeling_results,
#         sce_layer = ori_sce_layer)
#
#
# spatialLIBD::run_app(
#     sce = sce,
#     sce_layer = ori_sce_layer,
#     modeling_results = ori_modeling_results,
#     sig_genes = ori_sig_genes,
#     sce_discrete_vars = c('GraphBased',
#         'Layer',
#         'Maynard',
#         'Martinowich',
#         paste0('SNN_k50_k', 4:28), colnames(spatial_clus))
# )
## It does! =)

paste0("c('", paste(colnames(spatial_clus), collapse = "', '"), "')")
# c('SpatialDE_PCA', 'SpatialDE_pool_PCA', 'HVG_PCA', 'pseudobulk_PCA', 'markers_PCA', 'SpatialDE_UMAP', 'SpatialDE_pool_UMAP', 'HVG_UMAP', 'pseudobulk_UMAP', 'markers_UMAP', 'SpatialDE_PCA_spatial', 'SpatialDE_pool_PCA_spatial', 'HVG_PCA_spatial', 'pseudobulk_PCA_spatial', 'markers_PCA_spatial', 'SpatialDE_UMAP_spatial', 'SpatialDE_pool_UMAP_spatial', 'HVG_UMAP_spatial', 'pseudobulk_UMAP_spatial', 'markers_UMAP_spatial')"

## Overwrite the file
save(
    sce,
    file = here::here(
        'data-raw',
        'Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata'
    )
)

## Reproducibility information
print('Reproducibility information:')
Sys.time()
proc.time()
options(width = 120)
session_info()

# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
#  setting  value
#  version  R version 3.6.1 Patched (2019-10-31 r77350)
#  os       CentOS Linux 7 (Core)
#  system   x86_64, linux-gnu
#  ui       X11
#  language (EN)
#  collate  en_US.UTF-8
#  ctype    en_US.UTF-8
#  tz       US/Eastern
#  date     2020-02-19
#
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
#  package              * version  date       lib source
#  assertthat             0.2.1    2019-03-21 [2] CRAN (R 3.6.1)
#  backports              1.1.5    2019-10-02 [1] CRAN (R 3.6.1)
#  Biobase              * 2.46.0   2019-10-29 [2] Bioconductor
#  BiocGenerics         * 0.32.0   2019-10-29 [1] Bioconductor
#  BiocParallel         * 1.20.1   2019-12-21 [1] Bioconductor
#  Biostrings             2.54.0   2019-10-29 [1] Bioconductor
#  bitops                 1.0-6    2013-08-17 [2] CRAN (R 3.6.1)
#  cli                    2.0.0    2019-12-09 [1] CRAN (R 3.6.1)
#  colorout             * 1.2-2    2019-10-31 [1] Github (jalvesaq/colorout@641ed38)
#  colorspace             1.4-1    2019-03-18 [2] CRAN (R 3.6.1)
#  crayon                 1.3.4    2017-09-16 [1] CRAN (R 3.6.1)
#  DelayedArray         * 0.12.2   2020-01-06 [2] Bioconductor
#  digest                 0.6.23   2019-11-23 [1] CRAN (R 3.6.1)
#  fansi                  0.4.0    2018-10-05 [1] CRAN (R 3.6.1)
#  GenomeInfoDb         * 1.22.0   2019-10-29 [1] Bioconductor
#  GenomeInfoDbData       1.2.2    2019-10-28 [2] Bioconductor
#  GenomicAlignments      1.22.1   2019-11-12 [1] Bioconductor
#  GenomicRanges        * 1.38.0   2019-10-29 [1] Bioconductor
#  ggplot2                3.2.1    2019-08-10 [1] CRAN (R 3.6.1)
#  glue                   1.3.1    2019-03-12 [1] CRAN (R 3.6.1)
#  gtable                 0.3.0    2019-03-25 [2] CRAN (R 3.6.1)
#  here                 * 0.1      2017-05-28 [1] CRAN (R 3.6.1)
#  htmltools              0.4.0    2019-10-04 [1] CRAN (R 3.6.1)
#  htmlwidgets            1.5.1    2019-10-08 [1] CRAN (R 3.6.1)
#  httpuv                 1.5.2    2019-09-11 [1] CRAN (R 3.6.1)
#  IRanges              * 2.20.1   2019-11-20 [1] Bioconductor
#  jsonlite               1.6.1    2020-02-02 [2] CRAN (R 3.6.1)
#  later                  1.0.0    2019-10-04 [1] CRAN (R 3.6.1)
#  lattice                0.20-38  2018-11-04 [3] CRAN (R 3.6.1)
#  lazyeval               0.2.2    2019-03-15 [2] CRAN (R 3.6.1)
#  lifecycle              0.1.0    2019-08-01 [1] CRAN (R 3.6.1)
#  magrittr               1.5      2014-11-22 [1] CRAN (R 3.6.1)
#  Matrix                 1.2-17   2019-03-22 [3] CRAN (R 3.6.1)
#  matrixStats          * 0.55.0   2019-09-07 [1] CRAN (R 3.6.1)
#  munsell                0.5.0    2018-06-12 [2] CRAN (R 3.6.1)
#  pillar                 1.4.3    2019-12-20 [1] CRAN (R 3.6.1)
#  pkgconfig              2.0.3    2019-09-22 [1] CRAN (R 3.6.1)
#  png                    0.1-7    2013-12-03 [2] CRAN (R 3.6.1)
#  promises               1.1.0    2019-10-04 [1] CRAN (R 3.6.1)
#  R6                     2.4.1    2019-11-12 [2] CRAN (R 3.6.1)
#  Rcpp                   1.0.3    2019-11-08 [1] CRAN (R 3.6.1)
#  RCurl                  1.98-1.1 2020-01-19 [2] CRAN (R 3.6.1)
#  rlang                  0.4.2    2019-11-23 [1] CRAN (R 3.6.1)
#  rmote                * 0.3.4    2019-10-31 [1] Github (cloudyr/rmote@fbce611)
#  rprojroot              1.3-2    2018-01-03 [2] CRAN (R 3.6.1)
#  Rsamtools              2.2.1    2019-11-06 [1] Bioconductor
#  rtracklayer            1.46.0   2019-10-29 [1] Bioconductor
#  S4Vectors            * 0.24.1   2019-12-01 [1] Bioconductor
#  scales                 1.1.0    2019-11-18 [2] CRAN (R 3.6.1)
#  servr                  0.15     2019-08-07 [1] CRAN (R 3.6.1)
#  sessioninfo          * 1.1.1    2018-11-05 [1] CRAN (R 3.6.1)
#  SingleCellExperiment * 1.8.0    2019-10-29 [2] Bioconductor
#  SummarizedExperiment * 1.16.1   2019-12-19 [1] Bioconductor
#  tibble                 2.1.3    2019-06-06 [1] CRAN (R 3.6.1)
#  withr                  2.1.2    2018-03-15 [2] CRAN (R 3.6.1)
#  xfun                   0.11     2019-11-12 [1] CRAN (R 3.6.1)
#  XML                    3.99-0.3 2020-01-20 [2] CRAN (R 3.6.1)
#  XVector                0.26.0   2019-10-29 [1] Bioconductor
#  zlibbioc               1.32.0   2019-10-29 [2] Bioconductor
#
# [1] /users/lcollado/R/3.6.x
# [2] /jhpce/shared/jhpce/core/conda/miniconda3-4.6.14/envs/svnR-3.6.x/R/3.6.x/lib64/R/site-library
# [3] /jhpce/shared/jhpce/core/conda/miniconda3-4.6.14/envs/svnR-3.6.x/R/3.6.x/lib64/R/library
