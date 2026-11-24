Spot-level `spatialLIBD` documentation
======================================

This document describes the spot-level portion of the shiny web application made by the  [`spatialLIBD`](https://bioconductor.org/packages/spatialLIBD) Bioconductor package. You can either find the documentation about this package through [Bioconductor](https://bioconductor.org/packages/spatialLIBD) or at the [`spatialLIBD` documentation website](http://lieberinstitute.github.io/spatialLIBD). Below we explain the options common across tabs and each of the tabs at the spot-level data.

## Slides and videos

You might find the following slides useful for understanding the features from this part of the web application. 

<iframe class="speakerdeck-iframe" frameborder="0" src="https://speakerdeck.com/player/dde92cd6dfc04f9589770e074915658f" title="BioTuring_spatialLIBD" allowfullscreen="true" style="border: 0px; background: padding-box padding-box rgba(0, 0, 0, 0.1); margin: 0px; padding: 0px; border-radius: 6px; box-shadow: rgba(0, 0, 0, 0.2) 0px 5px 40px; width: 100%; height: auto; aspect-ratio: 560 / 420;" data-ratio="1.3333333333333333"></iframe>

These slides were part of our 2021-04-27 webinar for BioTuring that you can watch on YouTube:

<iframe width="560" height="315" src="https://www.youtube.com/embed/S8884Kde-1U" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

A recording of an earlier version of this talk is also available on YouTube.

<iframe width="560" height="315" src="https://www.youtube.com/embed/aD2JU-vUv54" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

You might also be interested in this video demonstration of `spatialLIBD` for the [LIBD rstats club](http://research.libd.org/rstatsclub/).

<iframe width="560" height="315" src="https://www.youtube.com/embed/LZ2kvCiRVdM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

## Raw summary

Before the documentation, this tab displays the [SpatialExperiment](https://bioconductor.org/packages/SpatialExperiment) object that contains the spot-level data. It's basically useful to know that the data has been loaded and that you can start navigating the app. If you wish to download this data, use the following command.

```{r}
## Download spe data
spe <- spatialLIBD::fetch_data(type = 'spe')
```

Throughout the rest of this document, we'll refer to this object by the name `spe`.

## Controls for each tab

`Interactive`, `Static grid`, and `Edit image` each have their own left sidebar. `Documentation` uses the full width and has no sidebar. Settings persist when switching tabs, and the plotting tabs have independent feature, filter, palette, and appearance settings. Applied image edits and manual annotations are shared within the session.

The following controls appear where relevant:

* `Sample`: which sample to plot on the `Interactive` tab.
* `Histology image`: which image resolution to use in the `Interactive` tab.
* `Discrete variable`: which discrete variable (typically with the cluster labels) to visualize. We include the clusters:
  - the official manual annotation used extensively in our pilot study (DOI: [10.1038/s41593-020-00787-0](https://doi.org/10.1038/s41593-020-00787-0))
  - from the graph based clustering results produced by `spaceranger` one Visium slide at a time. These clusters are saved as `GraphBased`.
  - from your own manual annotation of the spots under `ManualAnnotation`.
  - resulting the manual annotation by Kristen R Maynard and Keri Martinowich using known markers and histology features. This was an initial exploration and was not analyzed in more detail.
  - resulting from using a shared nearest neighbors approach with 50 neighbors cut at 4 up to 28 clusters. These are `SNN_k50_k4` up to `SNN_k50_k28`.
  - described in Figure 7 from our paper (DOI: [10.1038/s41593-020-00787-0](https://doi.org/10.1038/s41593-020-00787-0)) such as `SpatialDE_PCA`, `SpatialDE_pool_PCA` and others.
* `Continuous feature(s)`: which gene(s) or continuous variable(s) (such as the cell count, the ratio of the mitochondrial chromosome expression) to visualize in the `Interactive` and `Static grid` tabs. Multiple choices may be selected, in which case `Combine features` controls the method used to combine information from all selected variables.
* `Combine features`: when selecting more than one continuous variable, the method used to combine information from all selected variables. See [the multi gene plots vignette](https://research.libd.org/spatialLIBD/articles/multi_gene_plots.html) for more information about these methods for combining multiple continuous variables.
  * `z_score`: to summarize multiple continuous variables, each is normalized to represent a Z-score. The multiple scores are then averaged.
  * `pca`: PCA dimension reduction is conducted on the matrix formed by the continuous variables, and the first PC is then used and multiplied by -1 if needed to have the majority of the values for PC1 to be positive. 
  * `sparsity`: the proportion of continuous variables with positive values for each spot is computed.
* `Assay`: typically counts or logcounts: the gene expression assay to use.
* `Embedding`: which reduced dimension to visualize on the `Interactive` tab. Only the first two dimensions will be shown.
* `Clusters to show`: optional filters on the discrete variable's values shown in the `Interactive` and `Static grid` tabs.
* `Show values greater than`: Values from the selected `Continuous feature(s)` at or below this threshold will not be displayed.
* `Maximum percentile`: Sets the maximum count to display based on percentile, with acceptable values in (0, 1]. For example, a value of 0.95 sets the top 5% of counts to the value at the 95th percentile. This can be useful to make the color scale more dynamic in the presence of a small set of high outliers. Capped values also determine threshold eligibility when coloring by discrete categories.

## Appearance-related options

Click `Appearance` / `Show settings` in the sidebar to reveal these controls. Changes apply immediately in `Interactive`; click `Update grid` to apply that tab's appearance settings to the `Static grid` and its download.

* `Cell/spot boundaries`: when boundaries were included in the prepared inputs, choose `Outlines` (default) to show their edges or `Filled` to color their interiors using the continuous or discrete values. Boundaries appear as you zoom into the spatial panels; at wider views, units are shown as points. This setting applies only to the spatial panels in `Interactive`. Static grids always use points.
* `Discrete palette`: choose colors for the categories of the selected `Discrete variable`, in the current plotting tab. Choices include supplied colors from a matching metadata column (for example, `BayesSpace_colors` for `BayesSpace`) and palettes with enough colors for the categories. Historical LIBD layer annotations retain their established colors.
* `Reverse discrete palette`: reverse the order of colors assigned from a generated discrete palette. This does not reverse supplied colors or the fixed colors for historical LIBD annotations.
* `Continuous palette`: choose the color scale for the selected continuous feature or combined score, in the current plotting tab. The default is the color-blind-friendly `viridis` palette; other options include `magma`, `inferno`, `plasma`, `cividis`, `rocket`, `mako`, `turbo`, and the custom `paper` scale.
* `Dark to light`: control the direction of the continuous palette. Checked by default, this maps lower values toward the dark end and higher values toward the light end of the default `viridis` scale. Unchecking it reverses the selected scale, including the custom `paper` scale.
* `Unit opacity`: set the opacity of spatial units (e.g. spots or cells) and embedding points from 0 (transparent) to 1 (opaque, the default). Lower values can help reveal the histology beneath spatial units. This setting applies to `Interactive` and `Static grid`; units excluded by filters remain hidden.
* `Image opacity`: set the histology background opacity in the spatial panels of `Interactive`, from 0 (hidden) to 1 (opaque). The default is 0.8. This is independent of `Unit opacity` and does not change the static grid's background.
* `Point size` (Interactive): multiply the automatically chosen point radius in `Interactive`. A value of 1 (default) uses the suggested radius, 2 doubles it, and 0.5 halves it. Automatic sizing uses the sample's local point spacing and the panel dimensions, with separate estimates for spatial and embedding panels. Filtering does not change the suggested size. This control does not resize segmentation boundaries. The `Static grid` has its own `Point size (mm)` control.
* `Legend symbol size (mm)`: change the size of point symbols in the discrete legend of the `Static grid`. This does not change the plotted spatial units or the interactive legends.

Annotation download and import are covered under `Interactive` below.

## Interactive

Explore one sample in linked panels: spatial views above and the selected embedding below, with continuous values on the left and discrete categories on the right. Choose `None` under `Embedding` to hide the lower panels. Its sidebar filters apply to all four panels. Selection and navigation tools sit above the linked panels; annotation actions and annotation import/export sit below them.

* **Navigate:** use `Pan` to drag, scroll or use the +/− buttons to zoom, and expand a panel for a closer look. Spatial panels share one view; embedding panels share another. `Reset view` fits the data again. Full-resolution histology reveals more detail as you zoom, and segmentation boundaries appear when available.
* **Inspect and select:** hover for unit details; click a unit, drag a rectangle with `Select`, or draw a region with `Lasso`. Selections highlight the same units across all panels. Hold Shift to add or Alt to remove units; use `Clear selection` to start over. The status shows visible and selected unit counts.
* **Annotate:** enter a `Manual annotation label` and click `Label selected units`, or enable `Annotate individual units on click`. `Undo last annotation` reverses the latest label change or annotation import. Choose `ManualAnnotation` as the discrete variable to see your labels.
* **Save and restore:** annotations belong to your current session, so use `Download annotations` before closing it. `Omit unlabeled units from CSV` is enabled by default. Restore labels with `Import annotations`; CSV files require `sample_id`, `spot_name`, and `ManualAnnotation` columns. Later imported files overwrite earlier labels for matching units.

## Static grid

Compare selected samples in a grid of spatial plots, colored by continuous values or discrete categories. Units are always shown as points; use `Interactive` for zooming, selection, and segmentation boundaries.

* **Choose and arrange:** set `Color by` to `Continuous` or `Discrete`, choose `Samples` in the desired display order, and choose the number of `Columns`. Rows are calculated automatically. Each spatial panel has a fixed 300 × 300 pixel area, so rearranging samples does not resize their maps; wide grids scroll horizontally. Grid settings are in its own sidebar; its sample selection and plotting settings are independent of `Interactive`. `Update grid` and `Download plot` sit above the plots.
* **Style:** toggle `Show histology` or `Image beside features`, and adjust `Point size (mm)`. Image-beside-feature mode gives each sample two full-size spatial panels. Titles and legends occupy separate space. `One shared legend` (checked by default) uses all displayed categories or one continuous color range across the selected samples; uncheck for sample-specific legends and ranges. The grid has its own low-resolution `Histology image` choice, including applied edited images. Its sidebar also controls features, filters, applicable palettes, unit opacity, and legend symbol size. Continuous features and thresholds determine eligibility even when coloring by discrete categories.
* **Update and download:** click `Update grid` after changing settings. `Download plot` exports the displayed grid using its saved settings, dimensions, and point sizes. The format is PDF when the average number of displayed units per sample is below 50,000, or a 300 dpi PNG otherwise.

## Edit image

Use `Preview sample` and `Low-resolution image` in this tab's sidebar to choose the preview. Adjustments preview immediately; click an information icon for details about an option. Additional controls are grouped under `Channels and contrast` and `Colors and smoothing`. `Reset adjustments` restores defaults, and `Download preview PDF` exports the current preview.

`Apply edits to all samples` creates an `edited_imaged` low-resolution variant for every sample; full-resolution pyramids remain unchanged. Choose that variant independently in the `Interactive` or `Static grid` sidebar to display it. Enable `Replace existing edited images` to overwrite prior variants. For sequential edits, select `edited_imaged` as the editor's input image before applying further adjustments.

```{r}
## Reproduce locally with
spatialLIBD::img_update_all()
```
