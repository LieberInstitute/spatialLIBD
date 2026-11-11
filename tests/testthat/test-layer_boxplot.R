test_that("layer boxplots return ggplots with the selected expression and groups", {
    f <- app_fixture()
    p <- layer_boxplot(sig_genes = f$sig, sce_layer = f$pb, group_var = "app_cluster")
    expect_s3_class(p, "ggplot")
    expect_equal(p$data$expression, as.numeric(logcounts(f$pb)[f$sig$gene_index[1], ]))
    expect_equal(as.character(p$data$group), as.character(f$pb$app_cluster))
    boxes <- ggplot2::ggplot_build(p)$data[[1]]
    expect_equal(boxes$middle, as.numeric(tapply(p$data$expression, p$data$group, median)))
})

test_that("pairwise comparisons retain high, low, and background colors", {
    f <- app_fixture()
    i <- which(f$sig$model_type == "pairwise")[1]
    comparison <- strsplit(f$sig$test[i], "-", fixed = TRUE)[[1]]
    p <- layer_boxplot(
        i = i, sig_genes = f$sig, sce_layer = f$pb,
        group_var = "app_cluster"
    )
    expect_true(all(p$data$box_color[p$data$group == comparison[1]] == "skyblue"))
    expect_true(all(p$data$point_color[p$data$group == comparison[2]] == "darkviolet"))
    expect_true(all(p$data$box_color[!p$data$group %in% comparison] == "grey80"))
})

test_that("long labels wrap for display without changing group identities", {
    f <- app_fixture()
    labels <- paste0(levels(f$pb$app_cluster), "_LongClusterName_ForAxisClippingTest")
    levels(f$pb$app_cluster) <- labels
    p <- layer_boxplot(sig_genes = f$sig, sce_layer = f$pb, group_var = "app_cluster", cex = 2.7)
    displayed <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
    expect_equal(gsub("\n", "", displayed, fixed = TRUE), labels)
    expect_true(any(grepl("\n", displayed, fixed = TRUE)))
    expect_equal(levels(p$data$group), labels)
})

test_that("repeated plot builds have identical jitter and preserve the random seed", {
    f <- app_fixture()
    p <- layer_boxplot(sig_genes = f$sig, sce_layer = f$pb, group_var = "app_cluster")
    set.seed(7)
    seed <- .GlobalEnv$.Random.seed
    first <- ggplot2::ggplot_build(p)$data[[2]]
    second <- ggplot2::ggplot_build(p)$data[[2]]
    expect_identical(first$x, second$x)
    expect_identical(.GlobalEnv$.Random.seed, seed)
    expect_equal(first$y, p$data$expression)
})
