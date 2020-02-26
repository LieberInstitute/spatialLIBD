Layer-level `spatialLIBD` documentation
=======================================

This document describes the layer-level portion of the shiny web application made by the  [`spatialLIBD`](https://bioconductor.org/packages/spatialLIBD) Bioconductor package. You can either find the documentation about this package through [Bioconductor](https://bioconductor.org/packages/spatialLIBD) or at the [`spatialLIBD` documentation website](http://lieberinstitute.github.io/spatialLIBD). Below we explain the options common across tabs and each of the tabs at the layer-level data. As explained in the documentation, the layer-level data is the result of pseudo-bulking the spot-level data to compress it, reduce sparsity and power more analyses.

## Raw summary

This tab displays the [SingleCellExperiment](https://bioconductor.org/packages/SingleCellExperiment) object that contains the layer-level data. It's basically useful to know that the data has been loaded and that you can start navigating the app. If you wish to download this data, use the following command.

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
  3. `anova`: changes among the layers (adjusting more the mean expression) using the data from either all layers (`full`) or after dropping the white matter layer (`noWM`) since the rest of the layers have a richer concentration of neurons.
* `Gene`: the gene to display. You can search your gene by typing either the symbol or the Ensembl gene ID.

## Reduced dim

In this panel you can visualize the layer-level data (`sce_layer`) across reduced dimensionality representations derived from the gene expression data from the layer-level pseudo-bulked data. Select which dimensionality reduction method to use with `Reduced Dimension` (PCA, TSNE, UMAP) then `Color by` to choose which variable to color data by.

```{r}
## Reproduce locally with
scater::plotReducedDim(sce_layer)
```

## Model boxplots

This tab allows you to make a boxplot of the `logcounts` gene expression from the layer-level data (`sce_layer`) for a given gene. The model result information displayed in the title of the plot is based on which `model results` you selected and whether you are using the short title version or not (controlled by a checkbox). We provide two different color scales you can use: the color blind friendly `viridis` as well as a custom one we used for the `paper`. Through the `Model test` selector, you can choose which particular comparison to display. For example, `Layer1` for the `enrichment` model means that you would display the results of comparing Layer1 against the rest of the layers. `Layer1-Layer2` for the `pairwise` model means that you would display the results of Layer1 being greater than Layer2, while `Layer2-Layer` is the reverse scenario. Under `pairwise`, the layers not used are display in gray.

Below the plot you can find the subset of the table of results  (`sig_genes` from earlier), sort the table by the different columns, and download it as a CSV if you want. For more details about what each of these columns mean, check the [`spatialLIBD` vignette documentation](http://LieberInstitute.github.io/spatialLIBD/articles/spatialLIBD.html#extract-significant-genes).

```{r}
## Reproduce locally with
spatialLIBD::layer_boxplot()
```

## Gene Set Enrichment

This tab allows you to upload a CSV file that has a particular format as illustrated [in this example file](https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/asd_sfari_geneList.csv). This CSV file should contain:

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

## stat correlation

If you have a single nucleus or single cell RNA-sequencing (snRNA-seq)  (scRNA-seq) dataset, you might group your cells into clusters. Once you do, you could compress the data by pseudo-bulking (like we did to go from `sce` to `sce_layer`). You could then compute `enrichment` (`pairwise`, `anova`) statistics for your cell clusters. If you do so, you can then upload a specially formatted CSV file just like the one in [this example file](https://github.com/LieberInstitute/spatialLIBD/blob/master/data-raw/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.csv). This file has:

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
