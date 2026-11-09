# Editing the Shiny app

`run_app()` remains the public entry point. It validates inputs and supplies golem
options. Only `app_ui()` and `app_server()` read those options; feature functions
receive explicit arguments. No feature sources another file or looks up variables
in its caller's environment.

## Where to edit

| File in R/ | Responsibility |
|---|---|
| `app_ui.R` | Navigation, page composition, resources, help |
| `app_server.R` | Session setup and connections between feature servers |
| `app_spot_controls.R` | Spot sidebar and reactive palette selection |
| `app_spot_plots.R` | Static cluster/expression plots, warnings, PDFs |
| `app_spot_grids.R` | Multi-sample grids and their Update snapshots |
| `app_spot_interactive.R` | Linked spatial/embedding panels, standalone expression widget |
| `app_annotations.R` | Manual-label state, click/lasso actions, import/export |
| `app_image_editor.R` | Image UI, defaults, reset, preview, applying edits |
| `app_layer_controls.R` | Pseudobulk summary and model/test/gene navigation |
| `app_layer_dimensions.R` | Pseudobulk embeddings and PDF |
| `app_layer_models.R` | Boxplots, model tables, CSV/PDF |
| `app_layer_enrichment.R` | Gene-set input, enrichment results, plots, exports |
| `app_layer_registration.R` | Query input, correlations, annotations, exports |
| `app_palettes.R` | Legacy dataset palettes and boxplot color definitions |
| `app_helpers.R` | Small common helpers for plots, warnings, tables, files |

UI and server functions for a feature are adjacent in its file. These are ordinary
Shiny registration functions rather than namespaced modules: keeping the existing
input/output IDs avoids breaking downstream customizations and recorded workflows.
The functions still establish explicit dependency boundaries and return named
reactives for testing and communication. Do not pass the
entire parent server environment into a feature.

## Data and state

- `images$spe()` owns the session's expression object and edited images.
- `annotations()` owns a vector of manual labels aligned to the original spot keys.
- The top-level `spe()` combines those images and labels for visualization. It does
  not modify the caller's input object. There is no mutable global app state.
- `colors$discrete()` and `colors$continuous()` expose palettes.
- `settings()` contains spatial display arguments. `app_gene_settings()` contains
  expression display arguments. These are shared by static and interactive paths.
- `interactive$gene_data()` supplies the current expression subset and threshold
  mask to annotation handling. It is computed independently of rendering.
- Grid reactives capture their plot, rows, columns, and export labels at the Update
  event. Their displays and downloads read that same snapshot.
- Layer feature servers take one SCE, its model/gene tables, and its grouping
  variable. They do not borrow the embedding's color variable for model grouping.

The annotation server takes a lazy callback to `interactive$gene_data()`. This
connects the two features without giving either ownership of the other's state;
the callback is evaluated only after the features have been registered.

## Display and export

GGplot and heatmap outputs share their calculation reactive with downloads.
Layer boxplots return a ggplot object whose axes participate in automatic layout.
Long labels wrap for display, and jitter uses a fixed seed without altering the
session's random state. PDF devices close on error.

`app_table()` defines common table options; each feature supplies its own data,
row-name setting, and ordering. CSV downloads retain the original whole-result
semantics rather than exporting only a browser-filtered page.

`app_linked_spot_plot()` delegates expression preparation to `vis_gene()`, so gene
matching, assay selection, multi-feature summaries, thresholding and percentile
capping have a single implementation. Stable spot keys link panels and identify
annotations. Embedding themes are local; rendering does not alter ggplot's global
theme. Without an embedding, the linked widget shows its two spatial panels.

## Defaults and compatibility

Keep scientific defaults and the `run_app()` signature stable. Image input values
and Reset use `app_image_defaults()`; preview and Apply use the same argument list.
Historical layer palettes live in `app_palettes.R`. `edited_imaged` is the existing
custom-image identifier (including its spelling), retained for compatibility.

The refactor intentionally fixes these inconsistencies:

1. Image preview uses the UI's `sample` input, not the nonexistent `sampleid`.
2. Model boxplots use the same grouping for display and download.
3. Image changes invalidate dependent plots instead of requiring another control
   to be toggled. Images remain session-local.
4. Grid settings, plots, and export labels stay synchronized until Update.
5. Click annotation is an event handler, independent of whether click text renders.
6. Stitched cluster plots drop excluded rows from the plotted data as well as from
   the object passed to the plotting workhorse (`vis_clus.R`).
7. Missing embeddings and too few matching registration genes get usable empty
   states rather than broken plots or opaque correlation errors.

The layout replaces repeated blank-line tags with a small CSS spacer and adds IDs
to tabsets for reliable browser navigation. Original control IDs, choices, default
values, and feature order are preserved.

## Verification

Run the focused suites from the repository root:

```r
devtools::load_all()
testthat::test_dir("tests/testthat", filter = "^app-(features|integration)$")
```

These small tests do not cover every mouse gesture or prove equivalence for every
possible dataset. Keep browser checks for linked selection, image alignment, and
custom-widget behavior when changing those features.
