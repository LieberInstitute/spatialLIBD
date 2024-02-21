library("spatialLIBD")
library("sessioninfo")

spe <- fetch_data("spatialDLPFC_Visium")

lobstr::obj_size(spe)
# 6.97 GB

## Subset to just 3 samples used in Figure 2A
spe <- spe[, spe$sample_id %in% c("Br8667_mid", "Br6522_ant", "Br6432_ant")]

lobstr::obj_size(spe)
# 777.93 MB

imgData(spe)
# DataFrame with 12 rows and 4 columns
#       sample_id    image_id   data scaleFactor
#     <character> <character> <list>   <numeric>
# 1    Br6432_ant      lowres   ####   0.0148894
# 2    Br6432_ant       hires   ####   0.0496315
# 3    Br6432_ant    detected   ####   0.0496315
# 4    Br6432_ant     aligned   ####   0.0496315
# 5    Br6522_ant      lowres   ####   0.0192517
# ...         ...         ...    ...         ...
# 8    Br6522_ant     aligned   ####   0.0641725
# 9    Br8667_mid      lowres   ####   0.0169181
# 10   Br8667_mid       hires   ####   0.0563936
# 11   Br8667_mid    detected   ####   0.0563936
# 12   Br8667_mid     aligned   ####   0.0563936

## Subset to just the lowres images
imgData(spe) <- imgData(spe)[imgData(spe)$image_id == "lowres", ]

lobstr::obj_size(spe)
# 540.88 MB

saveRDS(spe, file = here::here("data-raw", "spatialDLPFC_spe_subset_example.rds"))
system2("ls", paste("-lh", here::here("data-raw", "spatialDLPFC_spe_subset_example.rds")))
# -rw-r--r--@ 1 leocollado  staff   107M May 23 23:42 /Users/leocollado/Dropbox/Code/spatialLIBD/data-raw/spatialDLPFC_spe_subset_example.rds

## Reproducibility information
print("Reproducibility information:")
Sys.time()
proc.time()
options(width = 120)
session_info()
# ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
#  setting  value
#  version  R version 4.3.0 (2023-04-21)
#  os       macOS Ventura 13.3.1
#  system   aarch64, darwin20
#  ui       RStudio
#  language (EN)
#  collate  en_US.UTF-8
#  ctype    en_US.UTF-8
#  tz       Europe/London
#  date     2023-05-23
#  rstudio  2023.03.1+446 Cherry Blossom (desktop)
#  pandoc   2.17.1.1 @ /opt/homebrew/bin/pandoc
#
# ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
#  package                * version    date (UTC) lib source
#  AnnotationDbi            1.62.1     2023-05-02 [1] Bioconductor
#  AnnotationHub            3.8.0      2023-04-25 [1] Bioconductor
#  attempt                  0.3.1      2020-05-03 [1] CRAN (R 4.3.0)
#  beachmat                 2.16.0     2023-04-25 [1] Bioconductor
#  beeswarm                 0.4.0      2021-06-01 [1] CRAN (R 4.3.0)
#  benchmarkme              1.0.8      2022-06-12 [1] CRAN (R 4.3.0)
#  benchmarkmeData          1.0.4      2020-04-23 [1] CRAN (R 4.3.0)
#  Biobase                * 2.60.0     2023-04-25 [1] Bioconductor
#  BiocFileCache            2.8.0      2023-04-25 [1] Bioconductor
#  BiocGenerics           * 0.46.0     2023-04-25 [1] Bioconductor
#  BiocIO                   1.10.0     2023-04-25 [1] Bioconductor
#  BiocManager              1.30.20    2023-02-24 [1] CRAN (R 4.3.0)
#  BiocNeighbors            1.18.0     2023-04-25 [1] Bioconductor
#  BiocParallel             1.34.1     2023-05-08 [1] Bioconductor
#  BiocSingular             1.16.0     2023-04-25 [1] Bioconductor
#  biocthis                 1.11.1     2023-05-06 [1] Github (lcolladotor/biocthis@42dc8df)
#  BiocVersion              3.17.1     2022-12-20 [1] Bioconductor
#  Biostrings               2.68.1     2023-05-16 [1] Bioconductor
#  bit                      4.0.5      2022-11-15 [1] CRAN (R 4.3.0)
#  bit64                    4.0.5      2020-08-30 [1] CRAN (R 4.3.0)
#  bitops                   1.0-7      2021-04-24 [1] CRAN (R 4.3.0)
#  blob                     1.2.4      2023-03-17 [1] CRAN (R 4.3.0)
#  brio                     1.1.3      2021-11-30 [1] CRAN (R 4.3.0)
#  bslib                    0.4.2      2022-12-16 [1] CRAN (R 4.3.0)
#  cachem                   1.0.8      2023-05-01 [1] CRAN (R 4.3.0)
#  callr                    3.7.3      2022-11-02 [1] CRAN (R 4.3.0)
#  cli                      3.6.1      2023-03-23 [1] CRAN (R 4.3.0)
#  codetools                0.2-19     2023-02-01 [1] CRAN (R 4.3.0)
#  colorout                 1.2-2      2023-05-06 [1] Github (jalvesaq/colorout@79931fd)
#  colorspace               2.1-0      2023-01-23 [1] CRAN (R 4.3.0)
#  config                   0.3.1      2020-12-17 [1] CRAN (R 4.3.0)
#  cowplot                  1.1.1      2020-12-30 [1] CRAN (R 4.3.0)
#  crayon                   1.5.2      2022-09-29 [1] CRAN (R 4.3.0)
#  curl                     5.0.0      2023-01-12 [1] CRAN (R 4.3.0)
#  data.table               1.14.8     2023-02-17 [1] CRAN (R 4.3.0)
#  DBI                      1.1.3      2022-06-18 [1] CRAN (R 4.3.0)
#  dbplyr                   2.3.2      2023-03-21 [1] CRAN (R 4.3.0)
#  DelayedArray             0.26.2     2023-05-05 [1] Bioconductor
#  DelayedMatrixStats       1.22.0     2023-04-25 [1] Bioconductor
#  devtools               * 2.4.5      2022-10-11 [1] CRAN (R 4.3.0)
#  digest                   0.6.31     2022-12-11 [1] CRAN (R 4.3.0)
#  doParallel               1.0.17     2022-02-07 [1] CRAN (R 4.3.0)
#  dotCall64                1.0-2      2022-10-03 [1] CRAN (R 4.3.0)
#  dplyr                    1.1.2      2023-04-20 [1] CRAN (R 4.3.0)
#  dqrng                    0.3.0      2021-05-01 [1] CRAN (R 4.3.0)
#  DropletUtils             1.20.0     2023-05-08 [1] Bioconductor
#  DT                       0.28       2023-05-18 [1] CRAN (R 4.3.0)
#  edgeR                    3.42.2     2023-05-08 [1] Bioconductor
#  ellipsis                 0.3.2      2021-04-29 [1] CRAN (R 4.3.0)
#  ExperimentHub            2.8.0      2023-04-25 [1] Bioconductor
#  fansi                    1.0.4      2023-01-22 [1] CRAN (R 4.3.0)
#  fastmap                  1.1.1      2023-02-24 [1] CRAN (R 4.3.0)
#  fields                   14.1       2022-08-12 [1] CRAN (R 4.3.0)
#  filelock                 1.0.2      2018-10-05 [1] CRAN (R 4.3.0)
#  foreach                  1.5.2      2022-02-02 [1] CRAN (R 4.3.0)
#  fs                       1.6.2      2023-04-25 [1] CRAN (R 4.3.0)
#  generics                 0.1.3      2022-07-05 [1] CRAN (R 4.3.0)
#  GenomeInfoDb           * 1.36.0     2023-04-25 [1] Bioconductor
#  GenomeInfoDbData         1.2.10     2023-05-06 [1] Bioconductor
#  GenomicAlignments        1.36.0     2023-04-25 [1] Bioconductor
#  GenomicRanges          * 1.52.0     2023-04-25 [1] Bioconductor
#  ggbeeswarm               0.7.2      2023-04-29 [1] CRAN (R 4.3.0)
#  ggplot2                  3.4.2      2023-04-03 [1] CRAN (R 4.3.0)
#  ggrepel                  0.9.3      2023-02-03 [1] CRAN (R 4.3.0)
#  glue                     1.6.2      2022-02-24 [1] CRAN (R 4.3.0)
#  golem                    0.4.0      2023-03-12 [1] CRAN (R 4.3.0)
#  gridExtra                2.3        2017-09-09 [1] CRAN (R 4.3.0)
#  gtable                   0.3.3      2023-03-21 [1] CRAN (R 4.3.0)
#  HDF5Array                1.28.1     2023-05-01 [1] Bioconductor
#  here                     1.0.1      2020-12-13 [1] CRAN (R 4.3.0)
#  hms                      1.1.3      2023-03-21 [1] CRAN (R 4.3.0)
#  htmltools                0.5.5      2023-03-23 [1] CRAN (R 4.3.0)
#  htmlwidgets              1.6.2      2023-03-17 [1] CRAN (R 4.3.0)
#  httpuv                   1.6.11     2023-05-11 [1] CRAN (R 4.3.0)
#  httr                     1.4.6      2023-05-08 [1] CRAN (R 4.3.0)
#  interactiveDisplayBase   1.38.0     2023-04-25 [1] Bioconductor
#  IRanges                * 2.34.0     2023-04-25 [1] Bioconductor
#  irlba                    2.3.5.1    2022-10-03 [1] CRAN (R 4.3.0)
#  iterators                1.0.14     2022-02-05 [1] CRAN (R 4.3.0)
#  jquerylib                0.1.4      2021-04-26 [1] CRAN (R 4.3.0)
#  jsonlite                 1.8.4      2022-12-06 [1] CRAN (R 4.3.0)
#  KEGGREST                 1.40.0     2023-04-25 [1] Bioconductor
#  later                    1.3.1      2023-05-02 [1] CRAN (R 4.3.0)
#  lattice                  0.21-8     2023-04-05 [1] CRAN (R 4.3.0)
#  lazyeval                 0.2.2      2019-03-15 [1] CRAN (R 4.3.0)
#  lifecycle                1.0.3      2022-10-07 [1] CRAN (R 4.3.0)
#  limma                    3.56.1     2023-05-08 [1] Bioconductor
#  lobstr                   1.1.2      2022-06-22 [1] CRAN (R 4.3.0)
#  locfit                   1.5-9.7    2023-01-02 [1] CRAN (R 4.3.0)
#  lubridate                1.9.2      2023-02-10 [1] CRAN (R 4.3.0)
#  magick                   2.7.4      2023-03-09 [1] CRAN (R 4.3.0)
#  magrittr                 2.0.3      2022-03-30 [1] CRAN (R 4.3.0)
#  maps                     3.4.1      2022-10-30 [1] CRAN (R 4.3.0)
#  Matrix                   1.5-4.1    2023-05-18 [1] CRAN (R 4.3.0)
#  MatrixGenerics         * 1.12.0     2023-04-25 [1] Bioconductor
#  matrixStats            * 0.63.0     2022-11-18 [1] CRAN (R 4.3.0)
#  memoise                  2.0.1      2021-11-26 [1] CRAN (R 4.3.0)
#  mime                     0.12       2021-09-28 [1] CRAN (R 4.3.0)
#  miniUI                   0.1.1.1    2018-05-18 [1] CRAN (R 4.3.0)
#  munsell                  0.5.0      2018-06-12 [1] CRAN (R 4.3.0)
#  paletteer                1.5.0      2022-10-19 [1] CRAN (R 4.3.0)
#  pillar                   1.9.0      2023-03-22 [1] CRAN (R 4.3.0)
#  pkgbuild                 1.4.0      2022-11-27 [1] CRAN (R 4.3.0)
#  pkgconfig                2.0.3      2019-09-22 [1] CRAN (R 4.3.0)
#  pkgload                  1.3.2      2022-11-16 [1] CRAN (R 4.3.0)
#  plotly                   4.10.1     2022-11-07 [1] CRAN (R 4.3.0)
#  png                      0.1-8      2022-11-29 [1] CRAN (R 4.3.0)
#  prettyunits              1.1.1      2020-01-24 [1] CRAN (R 4.3.0)
#  processx                 3.8.1      2023-04-18 [1] CRAN (R 4.3.0)
#  profvis                  0.3.8      2023-05-02 [1] CRAN (R 4.3.0)
#  promises                 1.2.0.1    2021-02-11 [1] CRAN (R 4.3.0)
#  prompt                   1.0.1      2023-05-06 [1] Github (gaborcsardi/prompt@7ef0f2e)
#  ps                       1.7.5      2023-04-18 [1] CRAN (R 4.3.0)
#  purrr                    1.0.1      2023-01-10 [1] CRAN (R 4.3.0)
#  R.cache                  0.16.0     2022-07-21 [1] CRAN (R 4.3.0)
#  R.methodsS3              1.8.2      2022-06-13 [1] CRAN (R 4.3.0)
#  R.oo                     1.25.0     2022-06-12 [1] CRAN (R 4.3.0)
#  R.utils                  2.12.2     2022-11-11 [1] CRAN (R 4.3.0)
#  R6                       2.5.1      2021-08-19 [1] CRAN (R 4.3.0)
#  rappdirs                 0.3.3      2021-01-31 [1] CRAN (R 4.3.0)
#  RColorBrewer             1.1-3      2022-04-03 [1] CRAN (R 4.3.0)
#  Rcpp                     1.0.10     2023-01-22 [1] CRAN (R 4.3.0)
#  RCurl                    1.98-1.12  2023-03-27 [1] CRAN (R 4.3.0)
#  rematch2                 2.1.2      2020-05-01 [1] CRAN (R 4.3.0)
#  remotes                  2.4.2      2021-11-30 [1] CRAN (R 4.3.0)
#  restfulr                 0.0.15     2022-06-16 [1] CRAN (R 4.3.0)
#  rhdf5                    2.44.0     2023-04-25 [1] Bioconductor
#  rhdf5filters             1.12.1     2023-04-30 [1] Bioconductor
#  Rhdf5lib                 1.22.0     2023-04-25 [1] Bioconductor
#  rjson                    0.2.21     2022-01-09 [1] CRAN (R 4.3.0)
#  rlang                    1.1.1      2023-04-28 [1] CRAN (R 4.3.0)
#  rprojroot                2.0.3      2022-04-02 [1] CRAN (R 4.3.0)
#  Rsamtools                2.16.0     2023-04-25 [1] Bioconductor
#  RSQLite                  2.3.1      2023-04-03 [1] CRAN (R 4.3.0)
#  rsthemes                 0.4.0      2023-05-06 [1] Github (gadenbuie/rsthemes@34a55a4)
#  rstudioapi               0.14       2022-08-22 [1] CRAN (R 4.3.0)
#  rsvd                     1.0.5      2021-04-16 [1] CRAN (R 4.3.0)
#  rtracklayer              1.60.0     2023-04-25 [1] Bioconductor
#  S4Arrays                 1.0.4      2023-05-14 [1] Bioconductor
#  S4Vectors              * 0.38.1     2023-05-02 [1] Bioconductor
#  sass                     0.4.6.9000 2023-05-06 [1] Github (rstudio/sass@f248fe5)
#  ScaledMatrix             1.8.1      2023-05-03 [1] Bioconductor
#  scales                   1.2.1      2022-08-20 [1] CRAN (R 4.3.0)
#  scater                   1.28.0     2023-04-25 [1] Bioconductor
#  scuttle                  1.10.1     2023-05-02 [1] Bioconductor
#  sessioninfo            * 1.2.2      2021-12-06 [1] CRAN (R 4.3.0)
#  shiny                    1.7.4      2022-12-15 [1] CRAN (R 4.3.0)
#  shinyWidgets             0.7.6      2023-01-08 [1] CRAN (R 4.3.0)
#  SingleCellExperiment   * 1.22.0     2023-04-25 [1] Bioconductor
#  spam                     2.9-1      2022-08-07 [1] CRAN (R 4.3.0)
#  sparseMatrixStats        1.12.0     2023-04-25 [1] Bioconductor
#  SpatialExperiment      * 1.10.0     2023-04-25 [1] Bioconductor
#  spatialLIBD            * 1.12.0     2023-04-27 [1] Bioconductor
#  statmod                  1.5.0      2023-01-06 [1] CRAN (R 4.3.0)
#  stringi                  1.7.12     2023-01-11 [1] CRAN (R 4.3.0)
#  stringr                  1.5.0      2022-12-02 [1] CRAN (R 4.3.0)
#  styler                   1.9.1      2023-03-04 [1] CRAN (R 4.3.0)
#  SummarizedExperiment   * 1.30.1     2023-05-01 [1] Bioconductor
#  suncalc                  0.5.1      2022-09-29 [1] CRAN (R 4.3.0)
#  testthat               * 3.1.8      2023-05-04 [1] CRAN (R 4.3.0)
#  tibble                   3.2.1      2023-03-20 [1] CRAN (R 4.3.0)
#  tidyr                    1.3.0      2023-01-24 [1] CRAN (R 4.3.0)
#  tidyselect               1.2.0      2022-10-10 [1] CRAN (R 4.3.0)
#  timechange               0.2.0      2023-01-11 [1] CRAN (R 4.3.0)
#  urlchecker               1.0.1      2021-11-30 [1] CRAN (R 4.3.0)
#  usethis                * 2.1.6      2022-05-25 [1] CRAN (R 4.3.0)
#  utf8                     1.2.3      2023-01-31 [1] CRAN (R 4.3.0)
#  vctrs                    0.6.2      2023-04-19 [1] CRAN (R 4.3.0)
#  vipor                    0.4.5      2017-03-22 [1] CRAN (R 4.3.0)
#  viridis                  0.6.3      2023-05-03 [1] CRAN (R 4.3.0)
#  viridisLite              0.4.2      2023-05-02 [1] CRAN (R 4.3.0)
#  withr                    2.5.0      2022-03-03 [1] CRAN (R 4.3.0)
#  XML                      3.99-0.14  2023-03-19 [1] CRAN (R 4.3.0)
#  xtable                   1.8-4      2019-04-21 [1] CRAN (R 4.3.0)
#  XVector                  0.40.0     2023-04-25 [1] Bioconductor
#  yaml                     2.3.7      2023-01-23 [1] CRAN (R 4.3.0)
#  zlibbioc                 1.46.0     2023-04-25 [1] Bioconductor
#
#  [1] /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library
#
# ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
