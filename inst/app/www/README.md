# spatialLIBD <img src="http://research.libd.org/spatialLIBD/reference/figures/logo.png" align="right" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/LieberInstitute/spatialLIBD.svg?branch=master)](https://travis-ci.org/LieberInstitute/spatialLIBD)
[![BioC
status](http://www.bioconductor.org/shields/build/release/bioc/spatialLIBD.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/spatialLIBD)
[![Codecov test
coverage](https://codecov.io/gh/LieberInstitute/spatialLIBD/branch/master/graph/badge.svg)](https://codecov.io/gh/LieberInstitute/spatialLIBD?branch=master)
<!-- badges: end -->

Welcome to the `spatialLIBD` [shiny](https://shiny.rstudio.com/) web application! This web application allows you to browse the LIBD human dorsolateral pre-frontal cortex (DLPFC) spatial transcriptomics data generated with the 10x Genomics Visium platform. Through the [R/Bioconductor package](https://bioconductor.org/packages/spatialLIBD) you can also download the data as well as visualize your own datasets using this web application. Please check the [bioRxiv pre-print](https://www.biorxiv.org/search/maynard%252Bcollado%252Bweber%252Bhicks%252Bmartinowich%252Bjaffe) for more details about this project.

## Study design

As a quick overview, the data presented here is from portion of the DLPFC that spans six neuronal layers plus white matter (**A**) for a total of three subjects with two pairs of spatially adjacent replicates (**B**). Each dissection of DLPFC was designed to span all six layers plus white matter (**C**). Using this web application you can explore the expression of known genes such as _SNAP25_ (**D**, a neuronal gene), _MOBP_ (**E**, an oligodendrocyte gene), and known layer markers from mouse studies such as _PCP4_ (**F**, a known layer 5 marker gene).

<img src="http://research.libd.org/spatialLIBD/reference/figures/paper_figure1.jpg" align="center" width="800px" />

This web application was built such that we could annotate the spots to layers as you can see under the **spot-level data** tab. Once we annotated each spot to a layer, we compressed the information by a pseudo-bulking approach into **layer-level data**. We then analyzed the expression through a set of models whose results you can also explore through this web application. Finally, you can upload your own gene sets of interest as well as layer enrichment statistics and compare them with our LIBD Human DLPFC Visium dataset.

If you are interested in running this web application locally, you can do so thanks to the `spatialLIBD` R/Bioconductor package that powers this web application as shown below.

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
#> 0.99.1, <URL: http://www.bioconductor.org/packages/spatialLIBD>.
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
