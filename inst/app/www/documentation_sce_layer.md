Layer-level `spatialLIBD` documentation
=======================================

This document describes the layer-level portion of the shiny web application made by the  [`spatialLIBD`](https://bioconductor.org/packages/spatialLIBD) Bioconductor package. You can either find the documentation about this package through [Bioconductor](https://bioconductor.org/packages/spatialLIBD) or at the [`spatialLIBD` documentation website](http://lieberinstitute.github.io/spatialLIBD). Below we explain the options common across tabs and each of the tabs at the layer-level data. As explained in the documentation, the layer-level data is the result of pseudo-bulking the spot-level data to compress it, reduce sparsity, and power more analyses.

## Slides and videos

You might find the following slides useful for understanding the features from this part of the web application. Particularly slides 10-12 and 15-22.

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/dde92cd6dfc04f9589770e074915658f" title="BioTuring_spatialLIBD" allowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 100%; height: auto; aspect-ratio: 560 / 420;" data-ratio="1.3333333333333333"></iframe>

These slides were part of our 2021-04-27 webinar for BioTuring that you can watch on YouTube:

<iframe width="560" height="315" src="https://www.youtube.com/embed/S8884Kde-1U" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

A recording of an earlier version of this talk is also available on YouTube.

<iframe width="560" height="315" src="https://www.youtube.com/embed/aD2JU-vUv54" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

You might also be interested in this video demonstration of `spatialLIBD` for the [LIBD rstats club](http://research.libd.org/rstatsclub/). Particularly starting at minute 26 with 25 seconds.

<iframe width="560" height="315" src="https://www.youtube.com/embed/LZ2kvCiRVdM?start=1584" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Raw summary

Before the documentation, this tab displays the [SingleCellExperiment](https://bioconductor.org/packages/SingleCellExperiment) object that contains the layer-level data. It's basically useful to know that the data has been loaded and that you can start navigating the app. If you wish to download this data, use the following command.

```{r}
## Download sce data
sce_layer <- spatialLIBD::fetch_data(type = 'sce_layer')
```

Throughout the rest of this document, we'll refer to this object by the name `sce_layer`.

This tab also shows the statistical modeling results, described below, that you can access locally and re-shape using the following code.

```{r}
## Reproduce locally with
modeling_results <- fetch_data('modeling_results')
sig_genes <-
        spatialLIBD::sig_genes_extract_all(
            n = nrow(sce_layer),
            modeling_results = modeling_results,
            sce_layer = sce_layer
        )
```

## Common options

* `Model results`: the statistical modeling results to use. We computed three different types of models:
  1. `enrichment`: one layer against all the the other layers. Results in t-statistics.
  2. `pairwise`: one layer against another one. Results in t-statistics with two-sided p-values.
  3. `anova`: changes among the layers (adjusting for the mean expression) using the data from either all layers (`full`) or after dropping the white matter layer (`noWM`) since the rest of the layers have a richer concentration of neurons.

## Reduced dim

In this panel you can visualize the layer-level data (`sce_layer`) across reduced dimensionality representations derived from the gene expression data from the layer-level pseudo-bulked data. Select which dimensionality reduction method to use with `Reduced Dimension` (PCA, TSNE, UMAP, etc) then `Color by` to choose which variable to color data by. The options are:

* `c_k20_k7`, `c_k7_k7` and `c_k5_k7` which are shared nearest neighbors with either 20, 7 or 5 neighbors cut at 7 clusters.
* `kmeans_k7`: k-means clustering results using k = 7 clusters
* `layer_guess`, `layer_guess_reordered`, `layer_guess_reordered_short` and `spatialLIBD` are all based on our manual annotations which were used for pseudo-bulking the data.
* sample information such as the `subject` (donor brain), `replicate` (whether it's the first or second slide in a pair of spatial replicates), `position` (spatial replicate distance), `subject_position` (the six unique spatial replicate pairs), or `sample_id` (formerly `sample_name`) which is the sample ID.

```{r}
## Reproduce locally with
scater::plotReducedDim(sce_layer)
```

## Model boxplots

This tab allows you to make a boxplot of the `logcounts` gene expression from the layer-level data (`sce_layer`) for a given `gene`; you can search your gene by typing either the symbol or the Ensembl gene ID. The model result information displayed in the title of the plot is based on which `model results` you selected and whether you are using the short title version or not (controlled by a checkbox). We provide two different color scales you can use: the color blind friendly `viridis` as well as a custom one we used for the `paper`. Through the `Model test` selector, you can choose which particular comparison to display. For example, `Layer1` for the `enrichment` model means that you would display the results of comparing Layer1 against the rest of the layers. `Layer1-Layer2` for the `pairwise` model means that you would display the results of Layer1 being greater than Layer2, while `Layer2-Layer1` is the reverse scenario. Under `pairwise`, the layers not used are display in gray.

Below the plot you can find the subset of the table of results  (`sig_genes` from earlier), sort the table by the different columns, and download it as a CSV if you want. For more details about what each of these columns mean, check the [`spatialLIBD` vignette documentation](http://LieberInstitute.github.io/spatialLIBD/articles/spatialLIBD.html#extract-significant-genes).

```{r}
## Reproduce locally with
spatialLIBD::layer_boxplot()
```

## Gene Set Enrichment

This tab allows you to upload a CSV file that has a particular format as illustrated [in this example file](https://github.com/LieberInstitute/spatialLIBD/blob/devel/data-raw/asd_sfari_geneList.csv). This CSV file should contain:

* one column per gene set of interest labeled as column names on the first row,
* no row names, 
* and human Ensembl gene IDs as values in the cells. 

Once you have uploaded a CSV file following this specific format, you can then check if the genes on each of your gene sets are enriched among the statistics from `model results` (`enrichment`, etc) that have a false discovery rate (FDR) adjusted p-value less than `FDR cutoff` (0.1 by default).

Similar to the `Model boxplots` tab, you can interact with the results table or download it.

```{r}
## Reproduce locally with
spatialLIBD::gene_set_enrichment()
spatialLIBD::gene_set_enrichment_plot()
```

## Spatial registration

If you have a single nucleus or single cell RNA-sequencing (snRNA-seq)  (scRNA-seq) dataset, you might group your cells into clusters. Once you do, you could compress the data by pseudo-bulking (like we did to go from `spe` to `sce_layer`). You could then compute `enrichment` (`pairwise`, `anova`) statistics for your cell clusters. If you do so, you can then upload a specially formatted CSV file just like the one in [this example file](https://github.com/LieberInstitute/spatialLIBD/blob/devel/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv). This file has:

* column names,
* human Ensembl gene IDs as the row names (first column, no name for the column),
* statistics (numeric values) for the cells.

Once you have uploaded a CSV file following this specific format, you can then assess whether the correlation between your statistics and the ones from our layers for the subset of genes (Ensembl ids) present in both. The resulting heatmap and interactive correlation matrix (which again you can interact with and download) can be useful if you are in the process of labeling your sn/scRNA-seq clusters or simply want to compare them against the layer-specific data we have provided.

Finally, you can change the `Maximum correlation` for visualization purposes on the heatmap as it will change the dynamic range for the colors.

```{r}
## Reproduce locally with
spatialLIBD::layer_stat_cor()
spatialLIBD::layer_stat_cor_plot()
```
