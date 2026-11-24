# Numbered behavior coverage follows the app's current data and event paths.
# Layer-level checks 23--29 retain the same functionality as before.

test_that("01 sample selection restricts viewer data to the chosen sample", {
    f <- app_fixture()
    id <- unique(f$spe$sample_id)[2]
    d <- app_frame(sampleid = id)
    expect_setequal(d$key, f$spe$key[f$spe$sample_id == id])
})

test_that("02 background selection can display an edited low-resolution image", {
    with_app({
        app_set_inputs(session, editImg_brightness = 0, editImg_update = 1)
        source <- app_image_source(catalog(), interactive_controls$input$sample, "edited_imaged")
        expect_identical(source$type, "image")
        expect_true(all(app_image_raster(spe(), interactive_controls$input$sample, source)$image %in% c("#000000", "#000000ff")))
        app_set_inputs(session, imageid = "edited_imaged")
        expect_identical(app_viewer_message(session, "spatiallibd:appearance")$image$type, "image")
    })
})

test_that("03 discrete payloads describe the chosen annotation variable", {
    with_app({
        payload <- app_viewer_message(session)
        expect_identical(payload$discreteTitle, "app_cluster")
        expect_identical(as.character(interactive$frame()$labels), as.character(f$spe$app_cluster[interactive$frame()$i]))
        expect_setequal(vapply(payload$discreteLegend, `[[`, character(1), "label"),
            unique(as.character(f$spe$app_cluster[interactive$frame()$i])))
    })
})

test_that("04 reversing a discrete palette reverses category colors", {
    with_app({
        before <- interactive_controls$discrete()
        app_set_inputs(session, clustercolor_direction = TRUE)
        expect_equal(unname(interactive_controls$discrete()), rev(unname(before)))
        expect_equal(names(interactive_controls$discrete()), names(before))
    })
})

test_that("05 gene and assay selection determine disk-backed expression values", {
    f <- app_fixture()
    for (name in c("counts", "logcounts")) {
        d <- app_frame(assayname = name)
        expect_equal(d$value, as.numeric(assay(f$source, name)[1, d$i]))
    }
})

test_that("06 continuous metadata is independent of the selected assay", {
    for (name in c("counts", "logcounts")) {
        d <- app_frame(geneid = "sum_umi", assayname = name)
        expect_equal(d$value, as.numeric(app_fixture()$source$sum_umi[d$i]))
    }
})

test_that("07 multi-feature methods preserve sample-wise scores", {
    f <- app_fixture()
    for (method in c("sparsity", "z_score", "pca")) {
        d <- app_frame(geneid = rownames(f$spe)[1:3], multi_gene_method = method)
        m <- t(as.matrix(logcounts(f$source)[1:3, d$i]))
        expected <- switch(method, sparsity = rowMeans(m > 0),
            z_score = rowMeans(scale(m)), pca = {
                fit <- stats::prcomp(m, center = TRUE, scale. = TRUE)
                fit$x[, 1] * if (mean(fit$rotation[, 1] > 0) < 0.5) -1 else 1
            })
        expect_equal(d$value, as.numeric(expected), tolerance = 1e-10)
    }
})

test_that("08 thresholds mask low values and percentile capping uses sample values", {
    original <- app_frame()$value
    filtered <- app_frame(minCount = median(original))$value
    expect_equal(is.na(filtered), original <= median(original))
    capped <- app_frame(cap_percentile = 0.5)$value
    cap <- sort(original)[max(1L, round(length(original) / 2))]
    expect_equal(capped, pmin(original, cap))
})

test_that("09 appearance changes preserve measurements and do not regenerate features", {
    with_app({
        before <- app_viewer_message(session)
        values <- interactive$frame()$value
        app_set_inputs(session, pointsize_scale = 2, alphalevel = 0.3, image_opacity = 0.4, boundary_style = "fill")
        expect_identical(app_viewer_message(session), before)
        expect_identical(interactive$frame()$value, values)
        appearance <- app_viewer_message(session, "spatiallibd:appearance")
        expect_equal(appearance$pointSizeScale, 2)
        expect_equal(appearance$opacity, 0.3)
        expect_equal(appearance$imageOpacity, 0.4)
        app_set_inputs(session, genecolor_direction = FALSE)
        expect_identical(interactive$frame()$value, values)
    })
})

test_that("10 small static grids download a nonempty PDF", {
    with_app({
        path <- output$downloadPlotGrid
        expect_true(file.exists(path))
        expect_gt(file.info(path)$size, 100)
        expect_equal(readChar(path, 4), "%PDF")
    })
})

test_that("11 linked views share one cell order across coordinates, embeddings and features", {
    with_app({
        payload <- app_viewer_message(session)
        units <- jsonlite::fromJSON(file.path(resources$root, basename(payload$dataset$url)))
        i <- interactive$frame()$i
        expect_identical(units$keys, as.character(f$spe$key[i]))
        expect_equal(units$x, unname(spatialCoords(f$spe)[i, "pxl_col_in_fullres"]))
        expect_equal(units$y, unname(spatialCoords(f$spe)[i, "pxl_row_in_fullres"]))
        expect_equal(units$embedding, unname(reducedDim(f$spe, interactive_controls$input$reduced_name)[i, 1:2]), tolerance = 1e-10)
        con <- file(file.path(resources$root, basename(payload$features)), "rb")
        on.exit(close(con), add = TRUE)
        values <- readBin(con, numeric(), n = length(i), size = 4, endian = "little")
        labels <- readBin(con, integer(), n = length(i), size = 4, endian = "little")
        expect_equal(values, interactive$frame()$value, tolerance = 1e-6)
        legend <- vapply(payload$discreteLegend, `[[`, character(1), "label")
        expect_identical(legend[labels + 1L], as.character(f$spe$app_cluster[i]))
        expect_equal(file.info(file.path(resources$root, basename(payload$features)))$size, 17 * length(i))
    })
})

test_that("12 cluster filters apply to all linked views without refitting scores", {
    with_app({
        before <- interactive$frame()$value
        chosen <- as.character(f$spe$app_cluster[1])
        app_set_inputs(session, cluster_subset = chosen)
        expected <- f$spe$key[f$spe$sample_id == interactive_controls$input$sample & f$spe$app_cluster == chosen]
        expect_setequal(interactive$gene_data()$key, expected)
        expect_identical(interactive$frame()$value, before)
        expect_equal(app_viewer_message(session)$shown, length(expected))
    })
})

test_that("13 clicking identifies a cell without annotating when disabled", {
    with_app({
        key <- f$spe$key[1]
        app_event(session, "viewer_click", key)
        expect_match(output$click, key, fixed = TRUE)
        expect_true(all(annotations() == "NA"))
    })
})

test_that("14 enabled click annotation changes exactly the clicked cell", {
    with_app({
        app_set_inputs(session, label_click = TRUE)
        app_event(session, "viewer_click", f$spe$key[1])
        expect_equal(which(annotations() == "reviewed"), 1L)
    })
})

test_that("15 selection annotation changes exactly the selected cells and supports undo", {
    with_app({
        app_event(session, "viewer_selection", f$spe$key[1:2])
        app_set_inputs(session, update_manual_ann = 1)
        expect_equal(which(annotations() == "reviewed"), 1:2)
        app_set_inputs(session, undo_manual_ann = 1)
        expect_true(all(annotations() == "NA"))
    })
})

test_that("16 annotations survive sample and feature changes", {
    with_app({
        app_event(session, "viewer_selection", f$spe$key[1])
        app_set_inputs(session, update_manual_ann = 1)
        app_set_inputs(session, sample = unique(f$spe$sample_id)[2], geneid = rowData(f$spe)$gene_search[2])
        app_set_inputs(session, sample = unique(f$spe$sample_id)[1])
        expect_equal(which(annotations() == "reviewed"), 1L)
    })
})

test_that("17 annotation export can include or omit unlabeled cells", {
    with_app({
        app_event(session, "viewer_selection", f$spe$key[1])
        app_set_inputs(session, update_manual_ann = 1)
        labeled <- read.csv(output$downloadData, na.strings = "")
        expect_equal(labeled$spot_name, colnames(f$spe)[1])
        expect_equal(labeled$ManualAnnotation, "reviewed")
        app_set_inputs(session, dropNA = FALSE)
        expect_equal(nrow(read.csv(output$downloadData)), ncol(f$spe))
    })
})

test_that("18 annotation import matches identity and later labels overwrite", {
    with_app({
        rows <- data.frame(sample_id = f$spe$sample_id[2:1], spot_name = colnames(f$spe)[2:1], ManualAnnotation = c("B", "A"))
        app_set_inputs(session, priorGuesses = app_csv(rows))
        expect_equal(annotations()[1:2], c("A", "B"))
        rows <- rows[2, ]; rows$ManualAnnotation <- "replacement"
        app_set_inputs(session, priorGuesses = app_csv(rows))
        expect_equal(annotations()[1:2], c("replacement", "B"))
    })
})

test_that("19 unified static grids follow the requested sample order", {
    with_app({
        calls <- character()
        original <- app_static_spatial
        testthat::local_mocked_bindings(app_static_spatial = function(spe, sample, ...) {
            calls <<- c(calls, sample)
            original(spe, sample, ...)
        }, .package = "spatialLIBD")
        order <- rev(unique(f$spe$sample_id))
        app_set_inputs(session, grid_samples = order, grid_kind = "discrete", grid_update = 1)
        invisible(grids$grid())
        expect_identical(calls, order)
    })
})

test_that("20 continuous static grids show the chosen gene in each sample", {
    f <- app_fixture()
    for (sample in unique(f$spe$sample_id)) {
        result <- app_static_plot(sample = sample, geneid = rownames(f$spe)[2])
        points <- result$p$layers[[1]]$data
        i <- which(f$spe$sample_id == sample)
        expect_equal(points$value, as.numeric(logcounts(f$source)[2, i]))
        expect_equal(points$x, unname(spatialCoords(f$spe)[i, "pxl_col_in_fullres"]))
        expect_equal(result$points, length(i))
    }
})

test_that("21 image preview uses the selected sample", {
    with_app({
        app_set_inputs(session, editImg_sample = unique(f$spe$sample_id)[2], editImg_brightness = 0)
        preview <- images$preview()
        expect_true(all(preview %in% c("#000000", "#000000ff")))
        expect_equal(dim(preview), dim(imgRaster(f$spe, sample_id = input$editImg_sample, image_id = "lowres")))
    })
})

test_that("22 applying image edits preserves originals for every sample", {
    with_app({
        app_set_inputs(session, editImg_brightness = 0, editImg_update = 1)
        for (id in unique(f$spe$sample_id)) {
            expect_equal(imgRaster(images$spe(), sample_id = id, image_id = "lowres"), imgRaster(f$spe, sample_id = id, image_id = "lowres"))
            expect_true(all(imgRaster(images$spe(), sample_id = id, image_id = "edited_imaged") %in% c("#000000", "#000000ff")))
        }
    })
})

test_that("23 pseudobulk overview describes the supplied object", {
    with_app({
        text <- output$layer_raw_summary
        expect_match(text, paste(nrow(f$pb), ncol(f$pb)), fixed = TRUE)
        expect_match(text, "logcounts", fixed = TRUE)
    })
})

test_that("24 pseudobulk reduced dimensions contain one point per column", {
    with_app({
        p <- dimensions$plot()
        expect_equal(nrow(p$data), ncol(f$pb))
        expect_true(all(is.finite(ggplot2::ggplot_build(p)$data[[1]]$x)))
    })
})

test_that("25 model test changes refresh the available gene choices", {
    with_app({
        # Capture messages that would be sent to the browser picker.
        messages <- list()
        session$sendInputMessage <- function(inputId, message) {
            messages[[inputId]] <<- message
        }
        tests <- unique(f$sig$test[f$sig$model_type == "enrichment"])
        app_set_inputs(session, layer_model_test = tests[2])
        expect_true("layer_geneid" %in% names(messages))
        expect_true(length(messages$layer_geneid) > 0)
    })
})

test_that("26 boxplot display and PDF use the same grouping variable", {
    with_app({
        # Record the actual calls; do not compare PDF bytes or jitter positions.
        groups <- character()
        original <- layer_boxplot
        testthat::local_mocked_bindings(layer_boxplot = function(..., group_var) {
            groups <<- c(groups, group_var)
            original(..., group_var = group_var)
        }, .package = "spatialLIBD")
        # A valid color variable distinct from the modeled grouping variable.
        app_set_inputs(session, layer_which_dim_color = "sample_id", layer_box_cex = 1.1)
        before <- models$plot()
        invisible(output$layer_downloadBoxplot)
        expect_equal(length(groups), 1L)
        expect_identical(models$plot(), before)
        expect_equal(unique(groups), "app_cluster")
    })
})

test_that("27 model tables restrict rows by selected gene or selected test", {
    with_app({
        gene <- models$gene_table()
        expect_true(nrow(gene) > 0)
        expect_true(all(gene$ensembl == rownames(f$pb)[1]))
        test <- models$test_table()
        expect_true(nrow(test) > 0)
        expect_true(all(test$test == input$layer_model_test))
        expect_true(all(test$model_type == input$layer_model))
    })
})

test_that("28 uploaded gene sets replace example sets in enrichment results", {
    with_app({
        app_set_inputs(session, geneSet = app_csv(data.frame(my_set = rownames(f$pb)[1:30])))
        result <- enrichment$result()
        expect_true(nrow(result) > 0)
        expect_equal(unique(result$ID), "my_set")
        expect_true(all(result$OR >= 0))
    })
})

test_that("29 registration matches uploaded genes by ID rather than row order", {
    with_app({
        tab <- f$models$enrichment
        stats <- tab[, grep("^t_stat_", names(tab)), drop = FALSE]
        rownames(stats) <- tab$ensembl
        path <- tempfile(fileext = ".csv")
        write.csv(stats[nrow(stats):1, ], path)
        app_set_inputs(session, externalTstat = list(datapath = path, name = basename(path)))
        result <- registration$correlations()
        # Each reference compared to itself should correlate perfectly.
        expect_equal(unname(diag(result[match(colnames(result), rownames(result)), , drop = FALSE])),
            rep(1, ncol(result)),
            tolerance = 1e-8
        )
    })
})
