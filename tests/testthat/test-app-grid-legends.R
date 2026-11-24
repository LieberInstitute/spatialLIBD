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
    # Legend categories must not depend on the presence or position of a raster layer.
    for (mode in c("none", "overlay", "paired")) {
        plots <- lapply(samples, function(sample) app_static_spatial(
            f$spe, sample, "app_cluster",
            list(geneid = rownames(f$spe)[1], assayname = "logcounts",
                minCount = -Inf, cap_percentile = 1, multi_gene_method = "z_score"),
            NULL, "discrete", list(discrete = palette), app_image_catalog(f$spe),
            "lowres", mode != "none", 2, 2, 1, mode == "paired"))
        expect_s3_class(app_static_grid(plots, length(samples), TRUE, "discrete")$p, "grob")
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
        expect_s3_class(app_static_grid(plots, length(samples), FALSE, "discrete")$p, "grob")
        expect_identical(ggplot2::ggplot_build(captured)$plot$scales$get_scales("colour")$get_limits(), "later")
        expect_identical(plots[[1]]$values,
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
    app_static_grid(plots, length(samples), TRUE, "continuous")
    scale <- ggplot2::ggplot_build(captured)$plot$scales$get_scales("colour")
    expect_equal(scale$get_limits(), c(9.5, 100.5))
    expect_identical(plots[[1]]$features$scales$get_scales("colour")$limits, c(9.5, 10.5))
    expect_identical(plots[[2]]$features$scales$get_scales("colour")$limits, c(99.5, 100.5))
})

test_that("map viewports retain their physical size across grid layouts and legends", {
    plots <- lapply(unique(app_fixture()$spe$sample_id), function(sample) app_static_plot(sample = sample))
    plots <- c(plots, plots[1])
    pixels <- function(unit) as.numeric(unit) * 96
    for (cols in 1:3) {
        for (shared in c(TRUE, FALSE)) {
            result <- app_static_grid(plots, cols, shared, "continuous")
            maps <- result$p$children[grepl("^map-feature-", names(result$p$children))]
            expect_length(maps, 3)
            expect_equal(result$rows, ceiling(3 / cols))
            for (map in maps) {
                expect_equal(pixels(map$vp$width), 300)
                expect_equal(pixels(map$vp$height), 300)
            }
            if (shared) {
                legend <- result$p$children[["legend-shared"]]
                expect_equal(pixels(legend$vp$y), pixels(maps[[1]]$vp$y))
                expect_gt(pixels(legend$vp$x), max(vapply(maps, function(g) pixels(g$vp$x) + 300, numeric(1))))
            }
        }
    }
    column <- app_static_grid(plots, 1, TRUE, "continuous")
    row <- app_static_grid(plots, 3, TRUE, "continuous")
    # Only the occupied map area changes: 300 px per map plus an 8 px gap.
    expect_equal(row$width - column$width, 2 * 308)
    expect_equal(column$height - row$height, 2 * 332)
})

test_that("paired image and feature maps each receive a full square", {
    f <- app_fixture()
    sample <- unique(f$spe$sample_id)[1]
    plot <- app_static_plot(sample = sample)
    plot$image <- plot$features # Any spatial ggplot exercises the paired layout.
    result <- app_static_grid(list(plot), 3, TRUE, "continuous")
    image <- result$p$children[["map-image-1"]]$vp
    feature <- result$p$children[["map-feature-1"]]$vp
    expect_equal(result$cols, 1)
    expect_equal(as.numeric(image$width) * 96, 300)
    expect_equal(as.numeric(feature$width) * 96, 300)
    expect_equal(as.numeric(feature$x - image$x) * 96, 308)
})

test_that("long discrete legends wrap without shrinking their map", {
    labels <- sprintf("Category %02d", 1:60)
    data <- data.frame(x = seq_along(labels), y = seq_along(labels), value = labels)
    plot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = data, ggplot2::aes(x, y, colour = value), show.legend = TRUE) +
        ggplot2::scale_colour_manual(values = stats::setNames(grDevices::hcl.colors(60), labels)) +
        ggplot2::ggtitle("Sample")
    result <- app_static_grid(list(list(features = plot, image = NULL, domain = c(0, 1),
        values = labels, guide_size = 2)),
        1, TRUE, "discrete")
    expect_lte(as.numeric(result$p$children[["legend-shared"]]$vp$height) * 96, 300)
    expect_equal(as.numeric(result$p$children[["map-feature-1"]]$vp$width) * 96, 300)
    expect_equal(result$height, 332)
})
