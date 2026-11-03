# Shared setup only; numbered behavior assertions live in test-app-features.R.
app_fixture <- local({
    fixture <- NULL
    function() {
        if (is.null(fixture)) {
            fixture <<- readRDS(testthat::test_path("fixtures", "app-spatialDLPFC.rds"))
            fixture$sig <<- sig_genes_extract_all(
                n = nrow(fixture$pb), modeling_results = fixture$models,
                sce_layer = fixture$pb
            )
        }
        fixture
    }
})

app_inputs <- function(f) {
    list(sample = unique(f$spe$sample_id)[1], imageid = "lowres",
         cluster = "app_cluster", clustercolor = "Polychrome::palette36",
         clustercolor_direction = FALSE, geneid = rowData(f$spe)$gene_search[1],
         assayname = "logcounts", multi_gene_method = "z_score",
         minCount = -Inf, cap_percentile = 1, genecolor = "viridis",
         genecolor_direction = TRUE, alphalevel = 1, pointsize = 2,
         guidepointsize = 2, auto_crop = FALSE,
         reduced_name = reducedDimNames(f$spe)[1],
         side_by_side_histology = FALSE, side_by_side_gene = FALSE,
         grid_update = 0, gene_grid_update = 0,
         grid_samples = unique(f$spe$sample_id),
         gene_grid_samples = unique(f$spe$sample_id),
         grid_nrow = 1, grid_ncol = 2, gene_grid_nrow = 1, gene_grid_ncol = 2,
         grid_spatial_clus = FALSE, grid_spatial_gene = FALSE,
         gene_plotly_cluster_subset = levels(f$spe$app_cluster),
         label_click = FALSE, label_click_gene = FALSE,
         label_manual_ann = "reviewed", label_manual_ann_gene = "reviewed",
         update_manual_ann = 0, update_manual_ann_gene = 0, dropNA = TRUE,
         layer_model = "enrichment", layer_model_test = f$sig$test[1],
         layer_geneid = rowData(f$pb)$gene_search[1],
         layer_which_dim = reducedDimNames(f$pb)[1],
         layer_which_dim_color = "app_cluster", layer_reduced_dim_ncomponents = 2,
         layer_reduced_dim_theme_size = 12, layer_reduced_dim_point_size = 2,
         layer_boxcolor = "viridis", layer_box_shortitle = TRUE,
         layer_box_cex = 1, layer_model_assayname = "logcounts",
         layer_gene_fdrcut = 0.1, enrichment_reverse = FALSE,
         layer_tstat_max = 0.81, layer_confidence_threshold = 0.25,
         layer_cutoff_merge_ratio = 0.25,
         editImg_channel = "", editImg_brightness = 100, editImg_saturation = 100,
         editImg_hue = 100, editImg_enhance = FALSE, editImg_normalize = FALSE,
         editImg_contrast_sharpen = NA_real_, editImg_quantize_max = NA_real_,
         editImg_quantize_dither = TRUE, editImg_equalize = FALSE,
         editImg_transparent_color = NA_character_, editImg_transparent_fuzz = 0,
         editImg_background_color = NA_character_, editImg_median_radius = NA_real_,
         editImg_negate = FALSE, editImg_overwrite = FALSE)
}

with_app <- function(code) {
    f <- app_fixture()
    opts <- list(spe = f$spe, sce_layer = f$pb, modeling_results = f$models,
                 sig_genes = f$sig, default_cluster = "app_cluster",
                 is_stitched = FALSE, datatype = "Visium")
    testthat::local_mocked_bindings(
        get_golem_options = function(which = NULL) {
            if (is.null(which)) opts else opts[[which]]
        }, .package = "golem"
    )
    body <- substitute(code)
    withCallingHandlers(shiny::testServer(app_server, {
        # Register expected events without requiring a browser to render a plot.
        session$userData$plotlyShinyEventIDs <- c(
            "plotly_click-plotly_histology", "plotly_selected-plotly_histology",
            "plotly_click-plotly_gene", "plotly_selected-plotly_gene")
        do.call(session$setInputs, c(app_inputs(f),
            list(geneSet = app_csv(data.frame(fixture_set = rownames(f$pb)[1:30])))))
        eval(body)
    }), warning = function(w) {
        # Plotly emits this once per trace on every redraw in the installed version.
        # Ignore only this known widget compatibility warning, not app warnings.
        if (startsWith(conditionMessage(w),
                       "'scattergl' objects don't have these attributes: 'hoveron'")) {
            invokeRestart("muffleWarning")
        }
    })
}

# A real Plotly payload; this tests server wiring, not mouse hit testing.
app_event <- function(session, event, keys, source = "plotly_histology") {
    payload <- jsonlite::toJSON(data.frame(key = keys), dataframe = "rows")
    do.call(session$setInputs, setNames(list(payload), paste(event, source, sep = "-")))
}

app_gene_plot <- function(f = app_fixture(), ...) {
    args <- modifyList(list(spe = f$spe, geneid = rownames(f$spe)[1],
                            minCount = -Inf, auto_crop = FALSE), list(...))
    do.call(vis_gene, args)
}

app_csv <- function(x) {
    path <- tempfile(fileext = ".csv")
    write.csv(x, path, row.names = FALSE)
    list(name = basename(path), size = file.info(path)$size,
         type = "text/csv", datapath = path)
}
