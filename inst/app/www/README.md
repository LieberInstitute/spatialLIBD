# spatialLIBD <img src="http://research.libd.org/spatialLIBD/reference/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/LieberInstitute/spatialLIBD.svg?branch=master)](https://travis-ci.org/LieberInstitute/spatialLIBD)
[![BioC
status](http://www.bioconductor.org/shields/build/release/bioc/spatialLIBD.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/spatialLIBD)
[![DOI](https://zenodo.org/badge/225913568.svg)](https://zenodo.org/badge/latestdoi/225913568)
[![Codecov test
coverage](https://codecov.io/gh/LieberInstitute/spatialLIBD/branch/master/graph/badge.svg)](https://codecov.io/gh/LieberInstitute/spatialLIBD?branch=master)
<!-- badges: end -->

Welcome to the `spatialLIBD` project\! It is composed of:

  - a [shiny](https://shiny.rstudio.com/) web application that we are
    hosting at
    [jhubiostatistics.shinyapps.io/spatialLIBD/](https://jhubiostatistics.shinyapps.io/spatialLIBD/)
    that can handle a
    [limited](https://github.com/LieberInstitute/spatialLIBD/issues/2)
    set of concurrent users,
  - a Bioconductor package at
    [bioconductor.org/packages/spatialLIBD](http://bioconductor.org/packages/spatialLIBD)
    (or from [here](http://research.libd.org/spatialLIBD/)) that lets
    you analyze the data and run a local version of our web application
    (with our data or yours),
  - and a [research
    article](https://www.biorxiv.org/search/maynard%252Bcollado%252Bweber%252Bhicks%252Bmartinowich%252Bjaffe)
    with the scientific knowledge we drew from this dataset. The
    analysis code for our project is available
    [here](https://github.com/LieberInstitute/HumanPilot/).

This web application allows you to browse the LIBD human dorsolateral
pre-frontal cortex (DLPFC) spatial transcriptomics data generated with
the 10x Genomics Visium platform. Through the [R/Bioconductor
package](https://bioconductor.org/packages/spatialLIBD) you can also
download the data as well as visualize your own datasets using this web
application. Please check the [bioRxiv
pre-print](https://www.biorxiv.org/search/maynard%252Bcollado%252Bweber%252Bhicks%252Bmartinowich%252Bjaffe)
for more details about this project.

If you tweet about this website, the data or the R package please use
the <code>\#spatialLIBD</code> hashtag. You can find previous tweets
that way as shown
<a href="https://twitter.com/search?q=%23spatialLIBD&src=typed_query">here</a>.
Thank you\!
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


## R/Bioconductor package

The `spatialLIBD` package contains functions for:

  - Accessing the spatial transcriptomics data from the LIBD Human Pilot
    project ([code on
    GitHub](https://github.com/LieberInstitute/HumanPilot)) generated
    with the Visium platform from 10x Genomics. The data is retrieved
    from [Bioconductor](http://bioconductor.org/)’s `ExperimentHub`.
  - Visualizing the spot-level spatial gene expression data and
    clusters.
  - Inspecting the data interactively either on your computer or through
    [jhubiostatistics.shinyapps.io/spatialLIBD/](https://jhubiostatistics.shinyapps.io/spatialLIBD/).

For more details, please check the [documentation
website](http://lieberinstitute.github.io/spatialLIBD) or the
Bioconductor package landing page
[here](https://bioconductor.org/packages/spatialLIBD).

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `spatialLIBD` using
from [Bioconductor](http://bioconductor.org/) the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("spatialLIBD")
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
library('spatialLIBD')

## Download the spot-level data
sce <- fetch_data(type = 'sce')
#> Loading objects:
#>   sce

## This is a SingleCellExperiment object
sce
#> class: SingleCellExperiment 
#> dim: 33538 47681 
#> metadata(1): image
#> assays(2): counts logcounts
#> rownames(33538): ENSG00000243485 ENSG00000237613 ... ENSG00000277475
#>   ENSG00000268674
#> rowData names(9): source type ... gene_search is_top_hvg
#> colnames(47681): AAACAACGAATAGTTC-1 AAACAAGTATCTCCCA-1 ...
#>   TTGTTTCCATACAACT-1 TTGTTTGTGTAAATTC-1
#> colData names(73): barcode sample_name ... pseudobulk_UMAP_spatial
#>   markers_UMAP_spatial
#> reducedDimNames(6): PCA TSNE_perplexity50 ... TSNE_perplexity80
#>   UMAP_neighbors15
#> spikeNames(0):
#> altExpNames(0):

## Note the memory size
pryr::object_size(sce)
#> 2.08 GB

## Remake the logo image with histology information
sce_image_clus(
    sce = sce,
    clustervar = 'layer_guess_reordered',
    sampleid = '151673',
    colors = libd_layer_colors,
    ... = ' DLPFC Human Brain Layers\nMade with github.com/LieberInstitute/spatialLIBD'
)
```

<img src="http://research.libd.org/spatialLIBD/reference/figures/README-access_data-1.png" width="600px" align="center" />

### Raw data

Below you can find the links to the raw data we received from 10x
Genomics.

| SampleID | h5\_filtered                                                                                    | h5\_raw                                                                                    | image\_hi                                                                                    | image\_lo                                                                                     | loupe                                                                       |
| -------: | :---------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------- |
|   151507 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151507_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151507_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151507_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151507.cloupe) |
|   151508 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151508_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151508_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151508_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151508_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151508.cloupe) |
|   151509 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151509_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151509_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151509_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151509_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151509.cloupe) |
|   151510 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151510_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151510_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151510_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151510_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151510.cloupe) |
|   151669 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151669_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151669_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151669_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151669_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151669.cloupe) |
|   151670 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151670_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151670_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151670_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151670_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151670.cloupe) |
|   151671 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151671_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151671_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151671_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151671_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151671.cloupe) |
|   151672 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151672_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151672_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151672_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151672_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151672.cloupe) |
|   151673 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151673_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151673_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151673_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151673_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151673.cloupe) |
|   151674 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151674_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151674_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151674_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151674_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151674.cloupe) |
|   151675 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151675_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151675_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151675_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151675_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151675.cloupe) |
|   151676 | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151676_filtered_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/h5/151676_raw_feature_bc_matrix.h5) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151676_tissue_hires_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/images/151676_tissue_lowres_image.png) | [AWS](https://spatial-dlpfc.s3.us-east-2.amazonaws.com/loupe/151676.cloupe) |

## Citation

Below is the citation output from using `citation('spatialLIBD')` in R.
Please run this yourself to check for any updates on how to cite
**spatialLIBD**.

``` r
citation('spatialLIBD')
#> 
#> Collado-Torres L, Maynard KR, Jaffe AE (2020). _LIBD Visium spatial
#> transcriptomics human pilot data inspector_. doi:
#> 10.18129/B9.bioc.spatialLIBD (URL:
#> https://doi.org/10.18129/B9.bioc.spatialLIBD),
#> https://github.com/LieberInstitute/spatialLIBD - R package version
#> 0.99.2, <URL: http://www.bioconductor.org/packages/spatialLIBD>.
#> 
#> Maynard KR, Collado-Torres L, Weber LM, Uytingco C, Williams SR, II
#> JLC, Barry BK, Tran MN, Besich Z, Tippani M, Chew J, Yin Y, Hyde TM,
#> Rao N, Hicks SC, Martinowich K, Jaffe AE (2020). "Transcriptome-scale
#> spatial gene expression in the human dorsolateral prefrontal cortex."
#> _bioRxiv_. doi: 10.1101/xxxyyy (URL: https://doi.org/10.1101/xxxyyy),
#> <URL: https://doi.org/10.1101/xxxyyy>.
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

Please note that the `spatialLIBD` was only made possible thanks to many
other R and bioinformatics software authors. We have cited their work
either in the pre-print or the vignette of the R package.
