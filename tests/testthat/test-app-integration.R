# Additional checks for state/dependency boundaries introduced by the refactor.
# Numbered feature assertions live in test-app-features.R.

test_that("the concise UI exposes current controls, defaults and help", {
    opts <- app_test_options()
    testthat::local_mocked_bindings(get_golem_options = function(which = NULL) {
        if (is.null(which)) opts else opts[[which]]
    }, .package = "golem")
    doc <- xml2::read_html(htmltools::renderTags(app_ui())$html)
    node <- function(id) {
        key <- if (id %in% c("geneid", "cluster_subset", "pointsize_scale", "alphalevel", "image_opacity", "boundary_style", "cluster", "clustercolor")) paste0("interactive_controls-", id) else id
        xml2::xml_find_first(doc, paste0("//*[@id='", key, "']"))
    }
    choices <- function(id) xml2::xml_attr(xml2::xml_find_all(node(id), ".//option"), "value")
    selected <- function(id) xml2::xml_attr(xml2::xml_find_all(node(id), ".//option[@selected]"), "value")
    tabs <- xml2::xml_text(xml2::xml_find_all(doc, "//ul[@id='spot_tabs']//a"))
    expect_identical(tabs, c("Documentation", "Interactive", "Static grid", "Edit image"))
    ids <- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
    expect_true(all(c(paste0("interactive_controls-", c("geneid", "cluster_subset", "pointsize_scale", "alphalevel", "image_opacity", "boundary_style")), "grid_kind") %in% ids))
    expect_false(any(c("pointsize", "auto_crop", "gene_grid_samples", "gene_plotly_cluster_subset", "grid_nrow") %in% ids))
    expect_identical(choices("boundary_style"), c("outline", "fill"))
    expect_identical(selected("boundary_style"), "outline")
    expect_false(is.na(xml2::xml_attr(node("grid_fit_to_units"), "checked")))
    expect_false(is.na(xml2::xml_attr(node("grid_shared_legend"), "checked")))
    expect_identical(choices("geneid"), "sum_umi") # Other genes use server-side search.
    expect_identical(choices("cluster"), opts$spe_discrete_vars)
    palettes <- paletteer::palettes_d_names
    usable <- palettes$length >= length(unique(opts$spe[[opts$default_cluster]]))
    supplied <- intersect(paste0(opts$default_cluster, "_colors"), colnames(colData(opts$spe)))
    expect_setequal(choices("clustercolor"), c(supplied,
        paste0(palettes$package[usable], "::", palettes$palette[usable])))
    expect_identical(xml2::xml_attr(node("pointsize_scale"), "value"), "1")
    for (id in "grid_ncol") {
        expect_identical(xml2::xml_attr(node(id), "min"), "1")
        expect_identical(xml2::xml_attr(node(id), "step"), "1")
    }
    for (id in c("alphalevel", "image_opacity")) {
        expect_identical(xml2::xml_attr(node(id), "data-min"), "0")
        expect_identical(xml2::xml_attr(node(id), "data-max"), "1")
        expect_identical(xml2::xml_attr(node(id), "data-step"), "0.05")
    }
    expect_identical(xml2::xml_attr(node("alphalevel"), "data-from"), "1")
    expect_identical(xml2::xml_attr(node("image_opacity"), "data-from"), "0.8")
    expect_gt(length(xml2::xml_find_all(doc, "//button[contains(@aria-label, 'About ')]")), 10)
    expect_length(xml2::xml_find_all(doc, "//details[contains(@class, 'sl-appearance')]/summary"), 4)
})

test_that("grid plots and download settings wait together for Update", {
    with_app({
        before <- grids$grid()
        app_set_inputs(session, target = "grid_controls",
            geneid = rowData(f$spe)$gene_search[2], grid_ncol = 1,
            grid_samples = unique(f$spe$sample_id)[2], multi_gene_method = "pca", grid_fit_to_units = FALSE,
            grid_shared_legend = FALSE
        )
        expect_identical(grids$grid(), before)
        app_set_inputs(session, grid_update = 1)
        expect_equal(grids$grid()$cols, 1)
        expect_false(grids$grid()$fit_to_units)
        expect_true(before$shared_legend)
        expect_false(grids$grid()$shared_legend)
        expect_match(grids$grid()$stem, unique(f$spe$sample_id)[2], fixed = TRUE)
        expect_false(identical(grids$grid()$stem, before$stem))
    })
})

test_that("grid layouts omit unused columns for fewer selected samples", {
    with_app({
        samples <- unique(f$spe$sample_id)
        for (n in 1:2) {
            for (shared in c(TRUE, FALSE)) {
                app_set_inputs(session, grid_samples = samples[seq_len(n)], grid_ncol = 3,
                    grid_shared_legend = shared, grid_update = paste(n, shared))
                expect_equal(grids$grid()$cols, n)
                expect_equal(grids$grid()$rows, 1)
            }
        }
    })
})

test_that("applied image changes invalidate spatial plots within the session", {
    with_app({
        original <- imgRaster(images$spe(), sample_id = interactive_controls$input$sample, image_id = "lowres")
        app_set_inputs(session, editImg_brightness = 0, editImg_update = 1)
        updated <- images$spe()
        expect_equal(imgRaster(updated, sample_id = interactive_controls$input$sample, image_id = "lowres"), original)
        expect_true("edited_imaged" %in% imgData(updated)$image_id)
        app_set_inputs(session, imageid = "edited_imaged")
        expect_true(all(images$preview() %in% c("#000000", "#000000ff")))
        image <- app_viewer_message(session, "spatiallibd:appearance")$image
        expect_true(file.exists(file.path(resources$root, basename(image$url))))
    })
})

test_that("image reset sends the same defaults used to construct the UI", {
    with_app({
        messages <- list()
        session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
        app_set_inputs(session, editImg_reset_menus = 1)
        expect_setequal(names(messages), paste0("editImg_", names(app_image_defaults())))
        expect_equal(as.numeric(messages$editImg_brightness$value), 100)
        expect_equal(messages$editImg_enhance$value, FALSE)
    })
})

test_that("selection annotation excludes cells masked by the expression threshold", {
    with_app({
        app_set_inputs(session, minCount = Inf)
        keys <- f$spe$key[f$spe$sample_id == interactive_controls$input$sample][1:2]
        app_event(session, "viewer_selection", keys)
        app_set_inputs(session, update_manual_ann = 1)
        expect_true(all(annotations() == "NA"))
        app_set_inputs(session, minCount = -Inf)
        app_set_inputs(session, update_manual_ann = 2)
        expect_setequal(f$spe$key[annotations() == "reviewed"], keys)
    })
})

test_that("layer table downloads contain their displayed data", {
    with_app({
        gene <- read.csv(output$layer_downloadModelTable, check.names = FALSE)
        all_genes <- read.csv(output$layer_downloadModelTable_full, check.names = FALSE)
        expect_equal(gene$ensembl, models$gene_table()$ensembl)
        expect_equal(gene$stat, models$gene_table()$stat, tolerance = 1e-12)
        expect_equal(all_genes$ensembl, models$test_table()$ensembl)
        expect_equal(all_genes$test, models$test_table()$test)
    })
})

test_that("spot-only apps initialize and do not register layer outputs", {
    with_app(
        {
            expect_match(output$raw_summary, "SpatialExperiment", fixed = TRUE)
            expect_true(file.exists(output$downloadPlotGrid))
            expect_error(output$layer_raw_summary, "hasn't been defined")
        },
        options = list(sce_layer = NULL, modeling_results = NULL, sig_genes = NULL)
    )
})

test_that("prepared point-only inputs work without a background image", {
    cells <- app_fixture()$source
    coords <- spatialCoords(cells)
    colnames(coords) <- c("x_centroid", "y_centroid")
    spatialCoords(cells) <- coords
    imgData(cells) <- NULL
    path <- tempfile("cells-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    app_prepare(cells, path)
    with_app({
        expect_equal(length(interactive$frame()$key), sum(f$spe$sample_id == interactive_controls$input$sample))
        expect_null(app_image_source(catalog(), interactive_controls$input$sample))
        expect_null(app_viewer_message(session, "spatiallibd:appearance")$image)
        expect_identical(app_viewer_message(session)$dataset$geometry, "")
    }, options = list(spe = open_app_dataset(path)$spe, datatype = "Xenium"))
})

test_that("overlap exclusions apply to viewer and static overview", {
    stitched <- app_fixture()$source
    stitched$exclude_overlapping <- FALSE
    stitched$exclude_overlapping[1] <- TRUE
    path <- tempfile("stitched-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    app_prepare(stitched, path)
    with_app({
        expect_false(f$spe$key[1] %in% interactive$frame()$key)
        p <- app_static_plot(f = modifyList(f, list(spe = spe())))
        expect_equal(p$points, sum(f$spe$sample_id == interactive_controls$input$sample) - 1L)
    }, options = list(spe = open_app_dataset(path)$spe))
})

test_that("blank reduced-dimension selections retain spatial data without embeddings", {
    with_app({
        app_set_inputs(session, reduced_name = "")
        payload <- app_viewer_message(session)
        expect_false(payload$dataset$hasEmbedding)
        units <- jsonlite::fromJSON(file.path(resources$root, basename(payload$dataset$url)))
        expect_length(units$embedding, 0L)
        expect_setequal(units$keys, f$spe$key[f$spe$sample_id == interactive_controls$input$sample])
    })
})

test_that("all model families can draw and download their selected gene", {
    set.seed(7)
    with_app({
        for (type in c("enrichment", "pairwise", "anova")) {
            row <- which(f$sig$model_type == type)[1]
            app_set_inputs(session,
                layer_model = type, layer_model_test = f$sig$test[row],
                layer_geneid = rowData(f$pb)$gene_search[f$sig$gene_index[row]]
            )
            device <- grDevices::dev.cur()
            seed <- .GlobalEnv$.Random.seed
            path <- output$layer_downloadBoxplot
            expect_equal(readChar(path, 4), "%PDF")
            expect_gt(file.info(path)$size, 100)
            expect_identical(grDevices::dev.cur(), device)
            expect_identical(.GlobalEnv$.Random.seed, seed)
        }
    })
})

test_that("annotation and image changes do not leak into a fresh session", {
    with_app({
        app_event(session, "viewer_selection", f$spe$key[1])
        app_set_inputs(session, update_manual_ann = 1, editImg_brightness = 0, editImg_update = 1)
        expect_equal(annotations()[1], "reviewed")
        expect_true("edited_imaged" %in% imgData(images$spe())$image_id)
    })
    with_app({
        expect_true(all(annotations() == "NA"))
        expect_false("edited_imaged" %in% imgData(images$spe())$image_id)
    })
})


test_that("stale clicks and selections from another sample cannot annotate cells", {
    with_app({
        app_set_inputs(session, label_click = TRUE)
        app_event(session, "viewer_click", f$spe$key[1], revision = -1)
        expect_true(all(annotations() == "NA"))
        app_event(session, "viewer_selection", f$spe$key[1], sample = unique(f$spe$sample_id)[2])
        app_set_inputs(session, update_manual_ann = 1)
        expect_true(all(annotations() == "NA"))
        app_event(session, "viewer_click", f$spe$key[1])
        expect_equal(which(annotations() == "reviewed"), 1L)
    })
})

test_that("dense grids download PNG and retain their saved format until Update", {
    with_app({
        drawn <- 50000
        original <- app_static_spatial
        testthat::local_mocked_bindings(app_static_spatial = function(...) {
            value <- original(...)
            value$points <- drawn # Exercise export policy without drawing 50k points.
            value
        }, .package = "spatialLIBD")
        app_set_inputs(session, grid_update = 1)
        expect_identical(grids$grid()$format, "png")
        path <- output$downloadPlotGrid
        expect_identical(readBin(path, "raw", 8), as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))
        con <- file(path, "rb")
        seek(con, 16)
        dimensions <- readBin(con, integer(), n = 2, size = 4, endian = "big")
        close(con)
        expect_equal(dimensions, floor(c(grids$grid()$width, grids$grid()$height) / 96 * 300))
        drawn <- 10
        app_set_inputs(session, grid_kind = "discrete")
        expect_identical(grids$grid()$format, "png")
        app_set_inputs(session, grid_update = 2)
        expect_identical(grids$grid()$format, "pdf")
        expect_identical(readChar(output$downloadPlotGrid, 4), "%PDF")
    })
})

test_that("fractional grid dimensions are rejected by the server", {
    with_app({
        app_set_inputs(session, grid_ncol = 0.5, grid_update = 1)
        expect_error(grids$grid(), "positive whole numbers")
    })
})
