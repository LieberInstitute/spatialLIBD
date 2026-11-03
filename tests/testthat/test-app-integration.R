# Additional checks for state/dependency boundaries introduced by the refactor.
# Keep the 29 feature assertions in their original, separately numbered file.

test_that("all original UI controls, choices, and defaults are preserved", {
    opts <- app_test_options()
    testthat::local_mocked_bindings(get_golem_options = function(which = NULL) {
        if (is.null(which)) opts else opts[[which]]
    }, .package = "golem")
    doc <- xml2::read_html(htmltools::renderTags(app_ui())$html)
    nodes <- xml2::xml_find_all(doc, "//input[@id] | //select[@id]")
    actual <- lapply(nodes, function(node) {
        list(
            tag = xml2::xml_name(node), type = xml2::xml_attr(node, "type"),
            value = xml2::xml_attr(node, "value"), checked = xml2::xml_attr(node, "checked"),
            min = xml2::xml_attr(node, "min"), max = xml2::xml_attr(node, "max"),
            step = xml2::xml_attr(node, "step"), multiple = xml2::xml_attr(node, "multiple"),
            options = xml2::xml_attr(xml2::xml_find_all(node, ".//option"), "value"),
            selected = xml2::xml_attr(xml2::xml_find_all(node, ".//option[@selected]"), "value")
        )
    })
    names(actual) <- xml2::xml_attr(nodes, "id")
    expected <- readRDS(test_path("fixtures", "app-ui-controls.rds"))
    expect_equal(actual, expected)
})

test_that("grid plots and download settings wait together for Update", {
    with_app({
        before <- grids$genes()
        session$setInputs(
            geneid = rowData(f$spe)$gene_search[2], gene_grid_ncol = 1,
            gene_grid_samples = unique(f$spe$sample_id)[2], multi_gene_method = "pca"
        )
        expect_identical(grids$genes(), before)
        session$setInputs(gene_grid_update = 1)
        expect_equal(grids$genes()$cols, 1)
        expect_match(grids$genes()$stem, unique(f$spe$sample_id)[2], fixed = TRUE)
        expect_false(identical(grids$genes()$stem, before$stem))
    })
})

test_that("applied image changes invalidate spatial plots within the session", {
    with_app({
        original <- imgRaster(images$spe(), sample_id = input$sample, image_id = "lowres")
        session$setInputs(editImg_brightness = 0, editImg_update = 1)
        updated <- images$spe()
        expect_equal(imgRaster(updated, sample_id = input$sample, image_id = "lowres"), original)
        expect_true("edited_imaged" %in% imgData(updated)$image_id)
        session$setInputs(imageid = "edited_imaged")
        expect_true(all(images$preview() %in% c("#000000", "#000000ff")))
        expect_true(file.exists(output$downloadPlotHistology))
    })
})

test_that("image reset sends the same defaults used to construct the UI", {
    with_app({
        messages <- list()
        session$sendInputMessage <- function(inputId, message) messages[[inputId]] <<- message
        session$setInputs(editImg_reset_menus = 1)
        expect_setequal(names(messages), paste0("editImg_", names(app_image_defaults())))
        expect_equal(as.numeric(messages$editImg_brightness$value), 100)
        expect_equal(messages$editImg_enhance$value, FALSE)
    })
})

test_that("gene-tab annotation excludes values masked by the expression threshold", {
    with_app({
        session$setInputs(minCount = Inf)
        keys <- f$spe$key[f$spe$sample_id == input$sample][1:2]
        app_event(session, "plotly_selected", keys, source = "plotly_gene")
        session$setInputs(update_manual_ann_gene = 1)
        expect_true(all(annotations() == "NA"))
        session$setInputs(minCount = -Inf)
        session$setInputs(update_manual_ann_gene = 2)
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
            expect_true(file.exists(output$downloadPlotGene))
            expect_error(output$layer_raw_summary, "hasn't been defined")
        },
        options = list(sce_layer = NULL, modeling_results = NULL, sig_genes = NULL)
    )
})

test_that("Xenium branch plots cells without requiring a background image", {
    cells <- app_fixture()$spe
    coords <- spatialCoords(cells)
    colnames(coords) <- c("x_centroid", "y_centroid")
    spatialCoords(cells) <- coords
    imgData(cells) <- NULL
    with_app(
        {
            expect_equal(nrow(plots$gene()$p$data), sum(f$spe$sample_id == input$sample))
            expect_true(is.null(app_background_image(images$spe(), input$sample, settings())))
            expect_error(output$editImg_plot, "hasn't been defined")
        },
        options = list(spe = cells, datatype = "Xenium")
    )
})

test_that("stitched plots exclude overlaps and disable automatic cropping", {
    stitched <- app_fixture()$spe
    stitched$exclude_overlapping <- FALSE
    stitched$exclude_overlapping[1] <- TRUE
    with_app(
        {
            expect_false(settings()$auto_crop)
            expect_false(f$spe$key[1] %in% plots$gene()$p$data$key)
            expect_false(f$spe$key[1] %in% plots$histology()$data$key)
        },
        options = list(spe = stitched, is_stitched = TRUE),
        inputs = list(auto_crop = TRUE)
    )
})

test_that("blank reduced-dimension selections still allow linked spatial panels", {
    with_app({
        session$setInputs(reduced_name = "")
        expect_true(nzchar(output$histology_plotly))
    })
})

test_that("all model families can draw and download their selected gene", {
    with_app({
        for (type in c("enrichment", "pairwise", "anova")) {
            row <- which(f$sig$model_type == type)[1]
            session$setInputs(
                layer_model = type, layer_model_test = f$sig$test[row],
                layer_geneid = rowData(f$pb)$gene_search[f$sig$gene_index[row]]
            )
            device <- grDevices::dev.cur()
            seed <- .Random.seed
            path <- output$layer_downloadBoxplot
            expect_equal(readChar(path, 4), "%PDF")
            expect_gt(file.info(path)$size, 100)
            expect_identical(grDevices::dev.cur(), device)
            expect_identical(.Random.seed, seed)
        }
    })
})

test_that("annotation and image changes do not leak into a fresh session", {
    with_app({
        app_event(session, "plotly_selected", f$spe$key[1])
        session$setInputs(update_manual_ann = 1, editImg_brightness = 0, editImg_update = 1)
        expect_equal(annotations()[1], "reviewed")
        expect_true("edited_imaged" %in% imgData(images$spe())$image_id)
    })
    with_app({
        expect_true(all(annotations() == "NA"))
        expect_false("edited_imaged" %in% imgData(images$spe())$image_id)
    })
})
