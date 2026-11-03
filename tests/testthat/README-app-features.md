# App behavior tests

`test-app-features.R` contains exactly 29 small tests, numbered to match the
feature inventory discussed before the app refactor. They describe intended
behavior, not a snapshot contract that every current behavior must be retained.
The original 29 tests remain the feature contract during the app refactor.
Their internal access paths were updated to use the feature servers; their
behavioral assertions were retained.

Run just this suite from the repository root:

```r
devtools::load_all()
testthat::test_file("tests/testthat/test-app-features.R")
```

Or run it with the other package tests using `devtools::test()`. The suite needs
`testthat >= 3.2.0` (for scoped mocks) and Shiny's `testServer()`. Package imports
must be installed, including `scrapper` on the current devel branch. No browser,
network access, ExperimentHub cache, or downloaded production objects are needed
for the tests themselves.

## Scope

Each test deliberately checks only one small, useful aspect of its feature.
For example, test 07 checks the sparsity summary, not all three summary methods;
test 20 checks grid contents, not every update-button interaction.

Tests use real plotting/data helpers where practical, and `testServer()` for
reactive and annotation behavior. The shared helper supplies golem options,
UI-like input defaults, and real-format Plotly event payloads. Configuration is
mocked because a mock Shiny session does not inherit a launched app's options.
Test 26 substitutes a plot-call recorder to compare display/download arguments;
it does not validate the substituted plot's appearance.

The tests do not establish mouse hit testing, client-side linked highlighting,
widget updates arriving in a real browser, visual alignment, all image effects,
all model types, error handling, or session isolation. In particular, test 11
checks shared plot keys, and tests 13–16 inject events rather than operate a mouse.
These remain useful manual/browser follow-ups.

The helper ignores only Plotly's repeated `scattergl`/`hoveron` compatibility
warning. Other warnings are left visible. Rendering happens through Shiny's
ordinary mock-session machinery; no application computations are disabled.

## Feature map

| Test | Feature | Small behavior checked |
|---|---|---|
| 01 | Sample selection | Plot keys belong to the selected sample |
| 02 | Background selection | Custom background can be created and plotted |
| 03 | Discrete variables | Plotted labels match source annotations |
| 04 | Discrete colors | Reversal reverses colors, not category names |
| 05 | Gene expression | Counts/logcounts match the selected assay |
| 06 | Continuous metadata | Values come from the metadata column |
| 07 | Multiple features | Sparsity equals proportion of positive values |
| 08 | Filtering/capping | Low values are masked; high values capped |
| 09 | Presentation | Style changes preserve measurements |
| 10 | Static plots | App download produces a PDF |
| 11 | Linked panels | Spot keys are shared across panels |
| 12 | Cluster subsets | Interactive expression excludes other groups |
| 13 | Identification | Click reports identity without changing labels |
| 14 | Click annotation | Exactly the clicked spot changes |
| 15 | Lasso annotation | Exactly the selected spots change |
| 16 | Annotation persistence | Labels survive sample/gene changes |
| 17 | Annotation export | Unlabeled rows can be kept or dropped |
| 18 | Annotation import | Identity, not row order, controls replacement |
| 19 | Cluster grids | Requested sample order and identities are used |
| 20 | Expression grids | Every sample shows the requested gene's values |
| 21 | Image preview | Preview follows the selected sample |
| 22 | Applying images | All samples get edited images; originals survive |
| 23 | Pseudobulk overview | Summary describes supplied dimensions/assays |
| 24 | Pseudobulk dimensions | One plotted point per pseudobulk column |
| 25 | Model navigation | Changing tests refreshes gene choices |
| 26 | Boxplots | Display and PDF use the modeled grouping |
| 27 | Model tables | Gene/test restrictions are honored |
| 28 | Gene-set enrichment | Uploaded sets replace example sets |
| 29 | Registration | Uploaded gene row order does not affect matching |

## Fixture provenance

`fixtures/app-spatialDLPFC.rds` is a small subset of these `fetch_data()` objects:

- `spatialDLPFC_Visium_example_subset`
- `spatialDLPFC_Visium_pseudobulk`
- `spatialDLPFC_Visium_modeling_results`

The fixture includes 40 common genes, 24 spots from each of two samples, the
matching model rows, and pseudobulk columns. Background images are resized to
fit within 128 x 128 pixels and scale factors adjusted accordingly. Measurements
and original cluster labels are retained. `app_cluster` aliases the spot-level
`BayesSpace_harmony_09` and pseudobulk `BayesSpace` annotations; `spatialLIBD` is
added to the pseudobulk object for the existing gene-extraction validator.
Manual annotations start as the app's literal string `"NA"`.

To regenerate, run `source("tests/testthat/fixtures/prepare-app-fixture.R")`
from the repository root. That separate preparation step downloads data. To use
already fetched files instead, set `APP_SOURCE_DATA` to a directory containing
`<fetch_data type>.rds` files before running it. The checked-in fixture is about
556 KiB; the source objects are much larger and are not checked in.

## Reviewing failures

First distinguish a bad expectation or fixture from an app defect. Do not
change assertions solely to make the existing app pass. Confirm the intended
behavior, record any bug, and fix implementation separately from this test-only
change. In particular, preview sample selection and display/download grouping
are intentionally tested even though the current source has inconsistencies.

## Initial result (2026-11-03)

Against devel commit `2c4b400922e4c0918c252d20c43abb02533ce73a`, the 29 tests
completed in approximately 42 seconds: 27 passed, two failed, no test errors
or unsuppressed test warnings. The failing expectations are retained:

- **21, selected-sample image preview:** the preview's dimensions match the first
  image rather than the selected second sample. `editImg_manipulations()` passes
  `input$sampleid` to `img_edit()`, but the UI input is named `sample`.
- **26, boxplot display/download agreement:** display uses `default_cluster`
  (`app_cluster` in the fixture), while the PDF uses `layer_which_dim_color`
  (`sample_id` after changing the color selection in this test).

Both behaviors were subsequently corrected by the refactor. Initial setup also reported
that some installed Bioconductor packages were built under a later R patch
version; these were startup warnings, not failures in the suite.


## Refactor integration checks

`test-app-integration.R` adds checks for the new dependency/state boundaries:
UI controls/defaults, grid update snapshots, applied image refresh and reset,
expression-filtered annotation, CSV/display agreement, supported data modes,
missing embeddings, all model-family boxplot downloads, and fresh-session state.
`fixtures/app-ui-controls.rds` captures the original UI's 66 input/select controls
and their choices/defaults on the same fixture; it is not an approved snapshot
of the new implementation. Tab IDs were added for stable browser navigation.

The UI contract fixture was generated from `R/app_ui.R` at commit
`2c4b400922e4c0918c252d20c43abb02533ce73a`, using `app_test_options()` and the
same expression fixture. Each entry records its tag/type, value/checked state,
limits/step, multi-select flag, options, and selected options. No screenshots or
random tab IDs are included.
