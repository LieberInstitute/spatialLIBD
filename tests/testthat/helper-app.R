# Shared setup only; numbered behavior assertions live in test-app-features.R.
app_fixture <- local({
    fixture <- NULL
    function() {
        if (is.null(fixture)) {
            fixture <<- readRDS(testthat::test_path("fixtures", "app-spatialDLPFC.rds"))
            fixture$source <<- fixture$spe
            path <- tempfile("app-fixture-")
            app_prepare(fixture$source, path)
            fixture$dataset <<- open_app_dataset(path)
            fixture$spe <<- fixture$dataset$spe
            fixture$sig <<- sig_genes_extract_all(
                n = nrow(fixture$pb), modeling_results = fixture$models,
                sce_layer = fixture$pb
            )
        }
        fixture
    }
})

app_inputs <- function(f) {
    values <- list(
        sample = unique(f$spe$sample_id)[1], imageid = "lowres",
        cluster = "app_cluster", clustercolor = "Polychrome::palette36",
        clustercolor_direction = FALSE, geneid = rowData(f$spe)$gene_search[1],
        assayname = "logcounts", multi_gene_method = "z_score",
        minCount = -Inf, cap_percentile = 1, genecolor = "viridis",
        genecolor_direction = TRUE, alphalevel = 1, pointsize_scale = 1,
        image_opacity = 0.8, boundary_style = "outline", viewer_ready = TRUE,
        guidepointsize = 2, reduced_name = reducedDimNames(f$spe)[1],
        cluster_subset = levels(f$spe$app_cluster),
        grid_update = 0, grid_samples = unique(f$spe$sample_id),
        grid_ncol = 2, grid_pointsize = 2,
        grid_kind = "continuous", grid_spatial = FALSE, grid_side_by_side = FALSE, grid_fit_to_units = TRUE,
        grid_shared_legend = TRUE,
        label_click = FALSE, label_manual_ann = "reviewed",
        update_manual_ann = 0, undo_manual_ann = 0, dropNA = TRUE,
        editImg_imageid = "lowres",
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
        editImg_negate = FALSE, editImg_overwrite = FALSE
    )
    shared <- c("cluster", "clustercolor", "clustercolor_direction", "geneid", "assayname", "multi_gene_method",
        "minCount", "cap_percentile", "genecolor", "genecolor_direction", "alphalevel", "cluster_subset")
    interactive <- c(shared, "sample", "imageid", "reduced_name", "pointsize_scale", "image_opacity", "boundary_style")
    grid <- c(shared, "guidepointsize")
    c(values[setdiff(names(values), union(interactive, grid))],
        app_namespace_inputs(values[interactive]), app_namespace_inputs(values[grid], "grid_controls"),
        list(editImg_sample = values$sample, grid_imageid = "lowres"))

}

app_namespace_inputs <- function(values, target = "interactive_controls") {
    controls <- c("sample", "imageid", "cluster", "geneid", "assayname", "multi_gene_method", "reduced_name",
        "cluster_subset", "minCount", "cap_percentile", "reset_filters", "clustercolor", "clustercolor_direction",
        "genecolor", "genecolor_direction", "alphalevel", "boundary_style", "image_opacity", "pointsize_scale", "guidepointsize")
    at <- names(values) %in% controls
    names(values)[at] <- paste0(target, "-", names(values)[at])
    values
}

app_set_inputs <- function(session, ..., target = "interactive_controls") {
    do.call(session$setInputs, app_namespace_inputs(list(...), target))
}

with_app <- function(code, options = list(), inputs = list()) {
    f <- app_fixture()
    opts <- modifyList(app_test_options(f), options, keep.null = TRUE)
    testthat::local_mocked_bindings(
        get_golem_options = function(which = NULL) {
            if (is.null(which)) opts else opts[[which]]
        }, .package = "golem"
    )
    body <- substitute(code)
    shiny::testServer(app_server, {
        # Capture the real binary-viewer protocol, without requiring WebGL.
        session$userData$viewer_messages <- list()
        session$sendCustomMessage <- function(type, message) {
            session$userData$viewer_messages <- c(session$userData$viewer_messages,
                list(list(type = type, payload = message)))
        }
        do.call(session$setInputs, modifyList(
            c(app_inputs(f), list(geneSet = app_csv(data.frame(fixture_set = rownames(f$pb)[1:30])))),
            app_namespace_inputs(inputs), keep.null = TRUE
        ))
        eval(body)
    })
}

app_viewer_message <- function(session, type = "spatiallibd:viewer") {
    messages <- Filter(function(x) identical(x$type, type), session$userData$viewer_messages)
    if (!length(messages)) {
        errors <- Filter(function(x) identical(x$type, "spatiallibd:error"), session$userData$viewer_messages)
        if (length(errors)) stop("Viewer error: ", tail(errors, 1)[[1]]$payload$message)
        stop("No viewer message of type ", type)
    }
    tail(messages, 1)[[1]]$payload
}

# Browser selection/click payloads contain stable IDs, sample and feature revision.
# This tests server validation and annotation wiring, not geometric hit testing.
app_event <- function(session, event, keys, sample = NULL, revision = NULL) {
    payload <- app_viewer_message(session)
    if (is.null(sample)) sample <- payload$dataset$sample
    if (is.null(revision)) revision <- payload$revision
    do.call(session$setInputs, setNames(list(list(keys = keys, sample = sample,
        revision = revision)), event))
}

app_frame <- function(f = app_fixture(), sampleid = unique(f$spe$sample_id)[1],
    geneid = rownames(f$spe)[1], assayname = "logcounts", minCount = -Inf,
    cap_percentile = 1, multi_gene_method = "z_score", clusters = NULL) {
    app_spot_frame(f$spe, sampleid, "app_cluster", list(geneid = geneid,
        assayname = assayname, minCount = minCount, cap_percentile = cap_percentile,
        multi_gene_method = multi_gene_method), clusters)
}

app_static_plot <- function(f = app_fixture(), sample = unique(f$spe$sample_id)[1],
    geneid = rownames(f$spe)[1], kind = "continuous") {
    app_static_spatial(f$spe, sample, "app_cluster",
        list(geneid = geneid, assayname = "logcounts", minCount = -Inf,
            cap_percentile = 1, multi_gene_method = "z_score"), NULL, kind,
        list(continuous = viridisLite::viridis(21), discrete = paletteer::paletteer_d("Polychrome::palette36")),
        app_image_catalog(f$spe), "lowres", FALSE, 2, 2, 1, FALSE)
}

# Suppress only known upstream preparation warnings, not application warnings.
app_prepare <- function(...) app_upstream(suppressMessages(prepare_app_inputs(...)))

app_upstream <- function(code) {
    withCallingHandlers(code, warning = function(w) {
        if (grepl("S4Vectors:::anyMissing|Matrix compression performs poorly with non-integers", conditionMessage(w))) {
            invokeRestart("muffleWarning")
        }
    })
}

app_csv <- function(x) {
    path <- tempfile(fileext = ".csv")
    write.csv(x, path, row.names = FALSE)
    list(
        name = basename(path), size = file.info(path)$size,
        type = "text/csv", datapath = path
    )
}

app_test_options <- function(f = app_fixture()) {
    list(
        spe = f$spe, sce_layer = f$pb, modeling_results = f$models,
        sig_genes = f$sig, default_cluster = "app_cluster",
        datatype = "Visium", title = "Test app",
        docs_path = system.file("app", "www", package = "spatialLIBD"),
        spe_discrete_vars = c("app_cluster", "ManualAnnotation"),
        spe_continuous_vars = "sum_umi"
    )
}
