# One small test for each feature in the manual-review inventory.
# Assertions express intended behavior, not snapshots of the current code.

test_that("01 sample selection restricts plot data to the chosen sample", {
    f <- app_fixture()
    id <- unique(f$spe$sample_id)[2]
    p <- app_gene_plot(sampleid = id)
    expect_setequal(p$data$key, f$spe$key[f$spe$sample_id == id])
})

test_that("02 background image selection can display a custom image", {
    f <- app_fixture()
    edited <- img_update_all(f$spe,
        image_id = "lowres", new_image_id = "custom",
        brightness = 0
    )
    p <- vis_clus(edited,
        clustervar = "app_cluster", image_id = "custom",
        auto_crop = FALSE
    )
    expect_setequal(p$data$key, f$spe$key[f$spe$sample_id == unique(f$spe$sample_id)[1]])
    expect_true(all(as.raster(img_edit(edited,
        sampleid = unique(edited$sample_id)[1],
        image_id = "custom"
    )) %in% c("#000000", "#000000ff")))
})

test_that("03 discrete plots retain the chosen annotation labels", {
    f <- app_fixture()
    p <- vis_clus(f$spe, clustervar = "app_cluster", auto_crop = FALSE)
    i <- match(p$data$key, f$spe$key)
    expect_equal(as.character(p$data$app_cluster), as.character(f$spe$app_cluster[i]))
})

test_that("04 reversing a discrete palette reverses category colors", {
    with_app({
        before <- colors$discrete()
        session$setInputs(clustercolor_direction = TRUE)
        expect_equal(unname(colors$discrete()), rev(unname(before)))
        expect_equal(names(colors$discrete()), names(before))
    })
})

test_that("05 gene and assay selection determine expression values", {
    f <- app_fixture()
    for (assay in c("counts", "logcounts")) {
        p <- app_gene_plot(assayname = assay)
        i <- match(p$data$key, f$spe$key)
        expect_equal(p$data$COUNT, as.numeric(assay(f$spe, assay)[1, i]))
    }
})

test_that("06 continuous metadata is independent of the gene assay", {
    f <- app_fixture()
    p <- vis_gene(f$spe,
        geneid = "sum_umi", assayname = "counts",
        minCount = -Inf, auto_crop = FALSE
    )
    expect_equal(p$data$COUNT, f$spe$sum_umi[match(p$data$key, f$spe$key)])
})

test_that("07 multiple-feature sparsity is the proportion of positive values", {
    f <- app_fixture()
    p <- vis_gene(f$spe,
        geneid = rownames(f$spe)[1:3], multi_gene_method = "sparsity",
        minCount = -Inf, auto_crop = FALSE
    )
    i <- match(p$data$key, f$spe$key)
    expected <- colMeans(as.matrix(logcounts(f$spe)[1:3, i]) > 0)
    expect_equal(p$data$COUNT, unname(expected))
})

test_that("08 filtering masks low values and percentile capping limits high ones", {
    original <- app_gene_plot()$data$COUNT
    filtered <- app_gene_plot(minCount = median(original))$data$COUNT
    expect_equal(is.na(filtered), original <= median(original))
    capped <- app_gene_plot(cap_percentile = 0.5)$data$COUNT
    expect_lte(max(capped), median(original))
    expect_equal(capped[original <= min(capped)], original[original <= min(capped)])
})

test_that("09 presentation settings do not change plotted measurements", {
    before <- app_gene_plot(point_size = 1, alpha = 0.3)
    after <- app_gene_plot(
        point_size = 3, alpha = 1,
        cont_colors = rev(viridisLite::viridis(21))
    )
    expect_equal(after$data[, c("key", "COUNT")], before$data[, c("key", "COUNT")])
    expect_equal(after$layers[[2]]$aes_params$size, 3)
})

test_that("10 a static plot can be downloaded as a nonempty PDF", {
    with_app({
        path <- output$downloadPlotGene
        expect_true(file.exists(path))
        expect_gt(file.info(path)$size, 100)
        expect_equal(readChar(path, 4), "%PDF")
    })
})

test_that("11 linked panels expose the same spot keys for spatial and reduced views", {
    with_app({
        widget <- jsonlite::fromJSON(output$histology_plotly, simplifyVector = FALSE)
        traces <- widget$x$data
        keys <- lapply(traces, function(x) unlist(x$key, use.names = FALSE))
        expected <- f$spe$key[f$spe$sample_id == input$sample]
        expect_true(length(keys) > 0)
        expect_true(all(expected %in% unlist(keys)))
        # A key must appear in more than one panel to support linked selection.
        expect_true(all(table(unlist(keys))[expected] >= 2))
    })
})

test_that("12 interactive gene cluster subsets exclude other clusters", {
    with_app({
        chosen <- as.character(f$spe$app_cluster[1])
        session$setInputs(gene_plotly_cluster_subset = chosen)
        invisible(output$gene_plotly)
        expected <- f$spe$key[f$spe$sample_id == input$sample & f$spe$app_cluster == chosen]
        expect_setequal(interactive$gene_data()$key, expected)
    })
})

test_that("13 clicking identifies a spot without annotating when disabled", {
    with_app({
        key <- f$spe$key[1]
        app_event(session, "plotly_click", key)
        expect_match(output$click, key, fixed = TRUE)
        expect_true(all(annotations() == "NA"))
    })
})

test_that("14 enabled click annotation changes exactly the clicked spot", {
    with_app({
        session$setInputs(label_click = TRUE)
        app_event(session, "plotly_click", f$spe$key[1])
        invisible(output$click)
        expect_equal(which(annotations() == "reviewed"), 1L)
    })
})

test_that("15 lasso annotation changes exactly the selected spots", {
    with_app({
        app_event(session, "plotly_selected", f$spe$key[1:2])
        session$setInputs(update_manual_ann = 1)
        expect_equal(which(annotations() == "reviewed"), 1:2)
    })
})

test_that("16 annotations survive sample and gene changes", {
    with_app({
        app_event(session, "plotly_selected", f$spe$key[1])
        session$setInputs(update_manual_ann = 1)
        session$setInputs(
            sample = unique(f$spe$sample_id)[2],
            geneid = rowData(f$spe)$gene_search[2]
        )
        session$setInputs(sample = unique(f$spe$sample_id)[1])
        expect_equal(which(annotations() == "reviewed"), 1L)
    })
})

test_that("17 annotation export can include or omit unlabeled spots", {
    with_app({
        app_event(session, "plotly_selected", f$spe$key[1])
        session$setInputs(update_manual_ann = 1)
        labeled <- read.csv(output$downloadData, na.strings = "")
        expect_equal(labeled$spot_name, colnames(f$spe)[1])
        expect_equal(labeled$ManualAnnotation, "reviewed")
        session$setInputs(dropNA = FALSE)
        expect_equal(nrow(read.csv(output$downloadData)), ncol(f$spe))
    })
})

test_that("18 annotation import matches spot identity and later labels overwrite", {
    with_app({
        rows <- data.frame(
            sample_id = f$spe$sample_id[2:1],
            spot_name = colnames(f$spe)[2:1], ManualAnnotation = c("B", "A")
        )
        session$setInputs(priorGuesses = app_csv(rows))
        expect_equal(annotations()[1:2], c("A", "B"))
        rows <- rows[2, ]
        rows$ManualAnnotation <- "replacement"
        session$setInputs(priorGuesses = app_csv(rows))
        expect_equal(annotations()[1:2], c("replacement", "B"))
    })
})

test_that("19 cluster grids follow the requested sample order", {
    f <- app_fixture()
    order <- rev(unique(f$spe$sample_id))
    plots <- vis_grid_clus(f$spe,
        clustervar = "app_cluster", sample_order = order,
        return_plots = TRUE, auto_crop = FALSE
    )
    expect_equal(names(plots), order)
    for (i in seq_along(plots)) {
        expect_setequal(plots[[i]]$data$key, f$spe$key[f$spe$sample_id == order[i]])
    }
})

test_that("20 expression grids show the chosen gene in each sample", {
    f <- app_fixture()
    plots <- vis_grid_gene(f$spe,
        geneid = rownames(f$spe)[2], minCount = -Inf,
        return_plots = TRUE, auto_crop = FALSE
    )
    for (p in plots) {
        i <- match(p$data$key, f$spe$key)
        expect_equal(p$data$COUNT, as.numeric(logcounts(f$spe)[2, i]))
    }
})

test_that("21 image preview uses the currently selected sample", {
    with_app({
        session$setInputs(sample = unique(f$spe$sample_id)[2], editImg_brightness = 0)
        preview <- images$preview()
        expect_true(all(preview %in% c("#000000", "#000000ff")))
        expected <- imgRaster(f$spe, sample_id = input$sample, image_id = "lowres")
        expect_equal(dim(preview), dim(expected))
    })
})

test_that("22 applying edited images preserves originals and adds images for all samples", {
    f <- app_fixture()
    edited <- img_update_all(f$spe,
        image_id = "lowres", new_image_id = "edited_imaged",
        brightness = 0
    )
    for (id in unique(f$spe$sample_id)) {
        expect_equal(
            imgRaster(edited, sample_id = id, image_id = "lowres"),
            imgRaster(f$spe, sample_id = id, image_id = "lowres")
        )
        expect_true(all(imgRaster(edited, sample_id = id, image_id = "edited_imaged")
        %in% c("#000000", "#000000ff")))
    }
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
        session$setInputs(layer_model_test = tests[2])
        expect_true("layer_geneid" %in% names(messages))
        expect_true(length(messages$layer_geneid) > 0)
    })
})

test_that("26 boxplot display and PDF use the same grouping variable", {
    with_app({
        # Record the actual calls; do not compare PDF bytes or jitter positions.
        groups <- character()
        testthat::local_mocked_bindings(layer_boxplot = function(..., group_var) {
            groups <<- c(groups, group_var)
            invisible(NULL) # This test checks wiring; other tests exercise real plots.
        }, .package = "spatialLIBD")
        # A valid color variable distinct from the modeled grouping variable.
        session$setInputs(layer_which_dim_color = "sample_id", layer_box_cex = 1.1)
        models$draw_boxplot()
        invisible(output$layer_downloadBoxplot)
        expect_gte(length(groups), 2L)
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
        session$setInputs(geneSet = app_csv(data.frame(my_set = rownames(f$pb)[1:30])))
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
        session$setInputs(externalTstat = list(datapath = path, name = basename(path)))
        result <- registration$correlations()
        # Each reference compared to itself should correlate perfectly.
        expect_equal(unname(diag(result[match(colnames(result), rownames(result)), , drop = FALSE])),
            rep(1, ncol(result)),
            tolerance = 1e-8
        )
    })
})
