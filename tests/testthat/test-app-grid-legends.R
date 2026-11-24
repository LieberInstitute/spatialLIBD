test_that("shared grid legends include categories absent from the first sample", {
    f <- app_fixture()
    samples <- unique(f$spe$sample_id)
    f$spe$app_cluster <- ifelse(f$spe$sample_id == samples[1], "first", "later")
    palette <- c(first = "#CC0000", later = "#0000CC")
    original <- cowplot::get_legend
    captured <- NULL
    legend <- NULL
    testthat::local_mocked_bindings(get_legend = function(plot, ...) {
        captured <<- plot
        legend <<- original(plot, ...)
        legend
    }, .package = "cowplot")
    # Exercise image/feature pairs as well as feature-only grids.
    for (paired in c(FALSE, TRUE)) {
        plots <- lapply(samples, function(sample) app_static_spatial(
            f$spe, sample, "app_cluster",
            list(geneid = rownames(f$spe)[1], assayname = "logcounts",
                minCount = -Inf, cap_percentile = 1, multi_gene_method = "z_score"),
            NULL, "discrete", list(discrete = palette), app_image_catalog(f$spe),
            "lowres", paired, 2, 2, 1, paired))
        expect_s3_class(app_static_grid(plots, 1, length(samples), TRUE, "discrete"), "ggplot")
        scale <- ggplot2::ggplot_build(captured)$plot$scales$get_scales("colour")
        expect_setequal(scale$get_limits(), names(palette))
        expect_equal(unname(scale$map(names(palette))), unname(palette))
        # Limits alone are insufficient: ggplot can omit absent-category glyphs.
        point_colors <- function(g) {
            c(if (inherits(g, "points")) g$gp$col,
                unlist(lapply(c(g$grobs, as.list(g$children)), point_colors)))
        }
        expect_equal(unname(grDevices::col2rgb(point_colors(legend), alpha = TRUE)),
            unname(grDevices::col2rgb(palette, alpha = TRUE)))
        captured <- NULL
        expect_s3_class(app_static_grid(plots, 1, length(samples), FALSE, "discrete"), "ggplot")
        expect_null(captured)
        expect_identical(plots[[1]]$features$layers[[1]]$data$value,
            rep("first", plots[[1]]$points))
    }
})

test_that("a shared continuous legend applies the combined sample range", {
    f <- app_fixture()
    samples <- unique(f$spe$sample_id)
    f$spe$legend_score <- ifelse(f$spe$sample_id == samples[1], 10, 100)
    original <- cowplot::get_legend
    captured <- NULL
    testthat::local_mocked_bindings(get_legend = function(plot, ...) {
        captured <<- plot
        original(plot, ...)
    }, .package = "cowplot")
    plots <- lapply(samples, function(sample) app_static_spatial(
        f$spe, sample, "app_cluster",
        list(geneid = "legend_score", assayname = "logcounts",
            minCount = -Inf, cap_percentile = 1, multi_gene_method = "z_score"),
        NULL, "continuous", list(continuous = viridisLite::viridis(21)),
        app_image_catalog(f$spe), "lowres", FALSE, 2, 2, 1, FALSE))
    app_static_grid(plots, 1, length(samples), TRUE, "continuous")
    scale <- ggplot2::ggplot_build(captured)$plot$scales$get_scales("colour")
    expect_equal(scale$get_limits(), c(9.5, 100.5))
    expect_identical(plots[[1]]$features$scales$get_scales("colour")$limits, c(9.5, 10.5))
    expect_identical(plots[[2]]$features$scales$get_scales("colour")$limits, c(99.5, 100.5))
})
