
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spatialLIBD <img src="http://research.libd.org/spatialLIBD/reference/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Bioc release
status](http://www.bioconductor.org/shields/build/release/data-experiment/spatialLIBD.svg)](https://bioconductor.org/checkResults/release/data-experiment-LATEST/spatialLIBD)
[![Bioc devel
status](http://www.bioconductor.org/shields/build/devel/data-experiment/spatialLIBD.svg)](https://bioconductor.org/checkResults/devel/data-experiment-LATEST/spatialLIBD)
[![Bioc downloads
rank](https://bioconductor.org/shields/downloads/release/spatialLIBD.svg)](http://bioconductor.org/packages/stats/bioc/spatialLIBD/)
[![Bioc
support](https://bioconductor.org/shields/posts/spatialLIBD.svg)](https://support.bioconductor.org/tag/spatialLIBD)
[![Bioc last
commit](https://bioconductor.org/shields/lastcommit/devel/data-experiment/spatialLIBD.svg)](http://bioconductor.org/checkResults/devel/data-experiment-LATEST/spatialLIBD/)
[![Bioc
dependencies](https://bioconductor.org/shields/dependencies/release/spatialLIBD.svg)](https://bioconductor.org/packages/release/data-experiment/html/spatialLIBD.html#since)
[![Codecov test
coverage](https://codecov.io/gh/LieberInstitute/spatialLIBD/branch/devel/graph/badge.svg)](https://codecov.io/gh/LieberInstitute/spatialLIBD?branch=devel)
[![R build
status](https://github.com/LieberInstitute/spatialLIBD/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/LieberInstitute/spatialLIBD/actions)
[![GitHub
issues](https://img.shields.io/github/issues/LieberInstitute/spatialLIBD)](https://github.com/LieberInstitute/spatialLIBD/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/LieberInstitute/spatialLIBD)](https://github.com/LieberInstitute/spatialLIBD/pulls)
[![DOI](https://zenodo.org/badge/225913568.svg)](https://zenodo.org/badge/latestdoi/225913568)

<!-- badges: end -->

Welcome to the `spatialLIBD` project! It is composed of:

- a [shiny](https://shiny.rstudio.com/) web application that we are
  hosting at
  [spatial.libd.org/spatialLIBD/](http://spatial.libd.org/spatialLIBD/)
  that can handle a
  [limited](https://github.com/LieberInstitute/spatialLIBD/issues/2) set
  of concurrent users,
- a Bioconductor package at
  [bioconductor.org/packages/spatialLIBD](http://bioconductor.org/packages/spatialLIBD)
  (or from [here](http://research.libd.org/spatialLIBD/)) that lets you
  analyze the data and run a local version of our web application (with
  our data or yours),
- and a [research article](https://doi.org/10.1038/s41593-020-00787-0)
  with the scientific knowledge we drew from this dataset. The analysis
  code for our project is available
  [here](https://github.com/LieberInstitute/HumanPilot/) and the high
  quality figures for the manuscript are available through
  [Figshare](https://doi.org/10.6084/m9.figshare.13623902.v1).

The web application allows you to browse the LIBD human dorsolateral
pre-frontal cortex (DLPFC) spatial transcriptomics data generated with
the 10x Genomics Visium platform. Through the [R/Bioconductor
package](https://bioconductor.org/packages/spatialLIBD) you can also
download the data as well as visualize your own datasets using this web
application. Please check the
[manuscript](https://doi.org/10.1038/s41593-020-00787-0) or [bioRxiv
pre-print](https://www.biorxiv.org/content/10.1101/2020.02.28.969931v1)
for more details about this project.

If you tweet about this website, the data or the R package please use
the <code>\#spatialLIBD</code> hashtag. You can find previous tweets
that way as shown
<a href="https://twitter.com/search?q=%23spatialLIBD&src=typed_query">here</a>.
Thank you!
<a href="https://twitter.com/intent/tweet?button_hashtag=spatialLIBD&ref_src=twsrc%5Etfw" class="twitter-hashtag-button" data-show-count="false">Tweet
\#spatialLIBD</a>
<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

## Study design

As a quick overview, the data presented here is from portion of the
DLPFC that spans six neuronal layers plus white matter (**A**) for a
total of three subjects with two pairs of spatially adjacent replicates
(**B**). Each dissection of DLPFC was designed to span all six layers
plus white matter (**C**). Using this web application you can explore
the expression of known genes such as *SNAP25* (**D**, a neuronal gene),
*MOBP* (**E**, an oligodendrocyte gene), and known layer markers from
mouse studies such as *PCP4* (**F**, a known layer 5 marker gene).

<img src="http://research.libd.org/spatialLIBD/reference/figures/paper_figure1.jpg" align="center" width="800px" />

This web application was built such that we could annotate the spots to
layers as you can see under the **spot-level data** tab. Once we
annotated each spot to a layer, we compressed the information by a
pseudo-bulking approach into **layer-level data**. We then analyzed the
expression through a set of models whose results you can also explore
through this web application. Finally, you can upload your own gene sets
of interest as well as layer enrichment statistics and compare them with
our LIBD Human DLPFC Visium dataset.

If you are interested in running this web application locally, you can
do so thanks to the `spatialLIBD` R/Bioconductor package that powers
this web application as shown below.

``` r
## Run this web application locally
spatialLIBD::run_app()

## You will have more control about the length of the
## session and memory usage.

## You could also use this function to visualize your
## own data given some requirements described
## in detail in the package vignette documentation
## at http://research.libd.org/spatialLIBD/.
```

## Shiny website mirrors

- [Main shiny application website](http://spatial.libd.org/spatialLIBD/)
  (note that the link must have a trailing slash `/` for it to work)
- [Shinyapps](https://libd.shinyapps.io/spatialLIBD/) This version has
  less RAM memory but is typically deployed using the latest version of
  `spatialLIBD`.

## Introductory material

If you prefer to watch a video overview of the `HumanPilot` project,
check the following journal club presentation of the main results.

<iframe width="560" height="315" src="https://www.youtube.com/embed/qloLbG5-IPM?si=1gO1fujrgSXPfa6F" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen data-external="1">
</iframe>

You might also be interested in the explainer video and [companion blog
post](https://lcolladotor.github.io/2024/05/23/humanpilot-first-spatially-resolved-transcriptomics-study-using-visium/)
as well as [the original Feb 29, 2020 blog
post](https://lcolladotor.github.io/2020/02/29/diving-together-into-the-unknown-world-of-spatial-transcriptomics/)
from when we first made this project public.

<iframe width="560" height="315" src="https://www.youtube.com/embed/HGioWKuI3ek?si=X-tqtZtcPSV-3uMt" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen data-external="1">
</iframe>

## R/Bioconductor package

The `spatialLIBD` package contains functions for:

- Accessing the spatial transcriptomics data from the LIBD Human Pilot
  project ([code on
  GitHub](https://github.com/LieberInstitute/HumanPilot)) generated with
  the Visium platform from 10x Genomics. The data is retrieved from
  [Bioconductor](http://bioconductor.org/)’s `ExperimentHub`.
- Visualizing the spot-level spatial gene expression data and clusters.
- Inspecting the data interactively either on your computer or through
  [spatial.libd.org/spatialLIBD/](http://spatial.libd.org/spatialLIBD/).

For more details, please check the [documentation
website](http://lieberinstitute.github.io/spatialLIBD) or the
Bioconductor package landing page
[here](https://bioconductor.org/packages/spatialLIBD).

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `spatialLIBD` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("spatialLIBD")
```

If you want to use the development version of `spatialLIBD`, you will
need to use the R version corresponding to the current
Bioconductor-devel branch as described in more detail on the
[Bioconductor
website](http://bioconductor.org/developers/how-to/useDevel/). Then you
can install `spatialLIBD` from GitHub using the following command.

``` r
BiocManager::install("LieberInstitute/spatialLIBD")
```

## Access the data

Through the `spatialLIBD` package you can access the processed data in
it’s final R format. However, we also provide a table of links so you
can download the raw data we received from 10x Genomics.

### Processed data

Using `spatialLIBD` you can access the Human DLPFC spatial
transcriptomics data from the 10x Genomics Visium platform. For example,
this is the code you can use to access the layer-level data. For more
details, check the help file for `fetch_data()`.

``` r
## Load the package
library("spatialLIBD")

## Download the spot-level data
spe <- fetch_data(type = "spe")

## This is a SpatialExperiment object
spe
#> class: SpatialExperiment 
#> dim: 33538 47681 
#> metadata(0):
#> assays(2): counts logcounts
#> rownames(33538): ENSG00000243485 ENSG00000237613 ... ENSG00000277475
#>   ENSG00000268674
#> rowData names(9): source type ... gene_search is_top_hvg
#> colnames(47681): AAACAACGAATAGTTC-1 AAACAAGTATCTCCCA-1 ...
#>   TTGTTTCCATACAACT-1 TTGTTTGTGTAAATTC-1
#> colData names(69): sample_id Cluster ... array_row array_col
#> reducedDimNames(6): PCA TSNE_perplexity50 ... TSNE_perplexity80
#>   UMAP_neighbors15
#> mainExpName: NULL
#> altExpNames(0):
#> spatialCoords names(2) : pxl_col_in_fullres pxl_row_in_fullres
#> imgData names(4): sample_id image_id data scaleFactor
```

``` r

## Note the memory size
lobstr::obj_size(spe)
#> 2.04 GB
```

``` r

## Remake the logo image with histology information
vis_clus(
    spe = spe,
    clustervar = "spatialLIBD",
    sampleid = "151673",
    colors = libd_layer_colors,
    ... = " DLPFC Human Brain Layers\nMade with research.libd.org/spatialLIBD/"
)
```

<img src="http://research.libd.org/spatialLIBD/reference/figures/README-access_data-1.png" width="100%" />

### Raw data

You can access all the raw data through
[Globus](http://research.libd.org/globus/) (`jhpce#HumanPilot10x`).
Furthermore, below you can find the links to the raw data we received
from 10x Genomics.

| SampleID | h5_filtered | h5_raw | image_full | image_hi | image_lo | loupe | HTML_report |
|---:|:---|:---|:---|:---|:---|:---|:---|
| 151507 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151507_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151507_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151507.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151507/151507_web_summary.html) |
| 151508 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151508_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151508_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151508_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151508_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151508_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151508.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151508/151508_web_summary.html) |
| 151509 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151509_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151509_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151509_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151509_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151509_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151509.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151509/151509_web_summary.html) |
| 151510 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151510_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151510_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151510_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151510_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151510_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151510.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151510/151510_web_summary.html) |
| 151669 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151669_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151669_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151669_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151669_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151669_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151669.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151669/151669_web_summary.html) |
| 151670 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151670_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151670_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151670_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151670_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151670_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151670.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151670/151670_web_summary.html) |
| 151671 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151671_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151671_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151671_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151671_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151671_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151671.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151671/151671_web_summary.html) |
| 151672 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151672_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151672_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151672_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151672_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151672_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151672.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151672/151672_web_summary.html) |
| 151673 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151673_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151673_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151673_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151673_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151673_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151673.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151673/151673_web_summary.html) |
| 151674 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151674_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151674_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151674_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151674_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151674_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151674.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151674/151674_web_summary.html) |
| 151675 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151675_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151675_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151675_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151675_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151675_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151675.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151675/151675_web_summary.html) |
| 151676 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151676_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151676_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151676_full_image.tif) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151676_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151676_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151676.cloupe) | [GitHub](https://github.com/LieberInstitute/HumanPilot/blob/master/10X/151676/151676_web_summary.html) |

## Citation

Below is the citation output from using `citation('spatialLIBD')` in R.
Please run this yourself to check for any updates on how to cite
**spatialLIBD**.

``` r
print(citation("spatialLIBD"), bibtex = TRUE)
#> To cite package 'spatialLIBD' in publications use:
#> 
#>   Pardo B, Spangler A, Weber LM, Hicks SC, Jaffe AE, Martinowich K,
#>   Maynard KR, Collado-Torres L (2022). "spatialLIBD: an R/Bioconductor
#>   package to visualize spatially-resolved transcriptomics data." _BMC
#>   Genomics_. doi:10.1186/s12864-022-08601-w
#>   <https://doi.org/10.1186/s12864-022-08601-w>,
#>   <https://doi.org/10.1186/s12864-022-08601-w>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {spatialLIBD: an R/Bioconductor package to visualize spatially-resolved transcriptomics data},
#>     author = {Brenda Pardo and Abby Spangler and Lukas M. Weber and Stephanie C. Hicks and Andrew E. Jaffe and Keri Martinowich and Kristen R. Maynard and Leonardo Collado-Torres},
#>     year = {2022},
#>     journal = {BMC Genomics},
#>     doi = {10.1186/s12864-022-08601-w},
#>     url = {https://doi.org/10.1186/s12864-022-08601-w},
#>   }
#> 
#>   Maynard KR, Collado-Torres L, Weber LM, Uytingco C, Barry BK,
#>   Williams SR, II JLC, Tran MN, Besich Z, Tippani M, Chew J, Yin Y,
#>   Kleinman JE, Hyde TM, Rao N, Hicks SC, Martinowich K, Jaffe AE
#>   (2021). "Transcriptome-scale spatial gene expression in the human
#>   dorsolateral prefrontal cortex." _Nature Neuroscience_.
#>   doi:10.1038/s41593-020-00787-0
#>   <https://doi.org/10.1038/s41593-020-00787-0>,
#>   <https://www.nature.com/articles/s41593-020-00787-0>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Transcriptome-scale spatial gene expression in the human dorsolateral prefrontal cortex},
#>     author = {Kristen R. Maynard and Leonardo Collado-Torres and Lukas M. Weber and Cedric Uytingco and Brianna K. Barry and Stephen R. Williams and Joseph L. Catallini II and Matthew N. Tran and Zachary Besich and Madhavi Tippani and Jennifer Chew and Yifeng Yin and Joel E. Kleinman and Thomas M. Hyde and Nikhil Rao and Stephanie C. Hicks and Keri Martinowich and Andrew E. Jaffe},
#>     year = {2021},
#>     journal = {Nature Neuroscience},
#>     doi = {10.1038/s41593-020-00787-0},
#>     url = {https://www.nature.com/articles/s41593-020-00787-0},
#>   }
#> 
#>   Huuki-Myers LA, Spangler A, Eagles NJ, Montgomergy KD, Kwon SH, Guo
#>   B, Grant-Peters M, Divecha HR, Tippani M, Sriworarat C, Nguyen AB,
#>   Ravichandran P, Tran MN, Seyedian A, Consortium P, Hyde TM, Kleinman
#>   JE, Battle A, Page SC, Ryten M, Hicks SC, Martinowich K,
#>   Collado-Torres L, Maynard KR (2024). "A data-driven single-cell and
#>   spatial transcriptomic map of the human prefrontal cortex."
#>   _Science_. doi:10.1126/science.adh1938
#>   <https://doi.org/10.1126/science.adh1938>,
#>   <https://doi.org/10.1126/science.adh1938>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {A data-driven single-cell and spatial transcriptomic map of the human prefrontal cortex},
#>     author = {Louise A. Huuki-Myers and Abby Spangler and Nicholas J. Eagles and Kelsey D. Montgomergy and Sang Ho Kwon and Boyi Guo and Melissa Grant-Peters and Heena R. Divecha and Madhavi Tippani and Chaichontat Sriworarat and Annie B. Nguyen and Prashanthi Ravichandran and Matthew N. Tran and Arta Seyedian and PsychENCODE Consortium and Thomas M. Hyde and Joel E. Kleinman and Alexis Battle and Stephanie C. Page and Mina Ryten and Stephanie C. Hicks and Keri Martinowich and Leonardo Collado-Torres and Kristen R. Maynard},
#>     year = {2024},
#>     journal = {Science},
#>     doi = {10.1126/science.adh1938},
#>     url = {https://doi.org/10.1126/science.adh1938},
#>   }
#> 
#>   Kwon SH, Parthiban S, Tippani M, Divecha HR, Eagles NJ, Lobana JS,
#>   Williams SR, Mark M, Bharadwaj RA, Kleinman JE, Hyde TM, Page SC,
#>   Hicks SC, Martinowich K, Maynard KR, Collado-Torres L (2023).
#>   "Influence of Alzheimer’s disease related neuropathology on local
#>   microenvironment gene expression in the human inferior temporal
#>   cortex." _bioRxiv_. doi:10.1101/2023.04.20.537710
#>   <https://doi.org/10.1101/2023.04.20.537710>,
#>   <https://www.biorxiv.org/content/10.1101/2023.04.20.537710v1>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {Influence of Alzheimer’s disease related neuropathology on local microenvironment gene expression in the human inferior temporal cortex},
#>     author = {Sang Ho Kwon and Sowmya Parthiban and Madhavi Tippani and Heena R. Divecha and Nicholas J. Eagles and Jashandeep S. Lobana and Stephen R. Williams and Michelle Mark and Rahul A. Bharadwaj and Joel E. Kleinman and Thomas M. Hyde and Stephanie C. Page and Stephanie C. Hicks and Keri Martinowich and Kristen R. Maynard and Leonardo Collado-Torres},
#>     year = {2023},
#>     journal = {bioRxiv},
#>     doi = {10.1101/2023.04.20.537710},
#>     url = {https://www.biorxiv.org/content/10.1101/2023.04.20.537710v1},
#>   }
```

Please note that the `spatialLIBD` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the spatialLIBD project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Development tools

- Continuous code testing is possible thanks to [GitHub
  actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
  through *[usethis](https://CRAN.R-project.org/package=usethis)*,
  *[remotes](https://CRAN.R-project.org/package=remotes)*,
  *[sysreqs](https://github.com/r-hub/sysreqs)* and
  *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)* customized
  to use [Bioconductor’s docker
  containers](https://www.bioconductor.org/help/docker/) and
  *[BiocCheck](https://bioconductor.org/packages/3.19/BiocCheck)*.
- Code coverage assessment is possible thanks to
  [codecov](https://codecov.io/gh) and
  *[covr](https://CRAN.R-project.org/package=covr)*.
- The [documentation
  website](http://lieberinstitute.github.io/spatialLIBD) is
  automatically updated thanks to
  *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
- The code is styled automatically thanks to
  *[styler](https://CRAN.R-project.org/package=styler)*.
- The documentation is formatted thanks to
  *[devtools](https://CRAN.R-project.org/package=devtools)* and
  *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.19/biocthis)*.

<a href="https://www.libd.org/"><img src="http://lcolladotor.github.io/img/LIBD_logo.jpg" width="250px"></a>

<center>
<script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=300&t=n&d=FRs8oQ9HVpMg6QLJJKAExpF8seGfPVlH-YOnwqUE8Hg'></script>
</center>
<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src="https://www.googletagmanager.com/gtag/js?id=G-QKT3SV9EFL"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'G-QKT3SV9EFL');
</script>
