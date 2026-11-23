test_that("grid fitting pads arbitrary coordinates and handles degenerate samples", {
    expect_equal(app_static_extent(c(-100, 100), c(0, 10)), c(-110, -10, 110, 20))
    expect_equal(app_static_extent(c(5, 5), c(-10, 10)), c(4, -11, 6, 11))
    image_extent <- c(-20, -30, 40, 50)
    expect_equal(app_static_extent(5, 7, fallback = image_extent), image_extent)
    expect_equal(app_static_extent(5, 7), c(4.95, 6.95, 5.05, 7.05))
})

test_that("fitting changes the viewport without moving images or following filters", {
    f <- app_fixture()
    sample <- unique(f$spe$sample_id)[1]
    catalog <- app_image_catalog(f$spe)
    image_extent <- c(-100000, -100000, 100000, 100000)
    catalog[[sample]]$sources$lowres$extent <- image_extent
    gene <- list(geneid = rownames(f$spe)[1], assayname = "logcounts",
        minCount = -Inf, cap_percentile = 1, multi_gene_method = "z_score")
    draw <- function(fit, clusters = NULL, feature = gene, image_id = "lowres", spatial = TRUE) {
        app_static_spatial(f$spe, sample, "app_cluster", feature, clusters, "continuous",
            list(continuous = viridisLite::viridis(21)), catalog, image_id,
            spatial, 2, 2, 1, FALSE, fit_to_units = fit)
    }
    fitted <- draw(TRUE)
    full <- draw(FALSE)
    i <- app_spot_indices(f$spe, sample)
    xy <- app_spot_coordinates(f$spe, i)
    expected <- app_static_extent(xy[, 1], xy[, 2])
    expect_equal(fitted$p$coordinates$limits$x, expected[c(1, 3)])
    expect_equal(sort(fitted$p$coordinates$limits$y), expected[c(2, 4)])
    expect_equal(full$p$coordinates$limits$x, image_extent[c(1, 3)])
    expect_equal(sort(full$p$coordinates$limits$y), image_extent[c(2, 4)])
    raster_extent <- function(p) unlist(p$layers[[1]]$geom_params[c("xmin", "ymin", "xmax", "ymax")], use.names = FALSE)
    expect_equal(raster_extent(fitted$p), image_extent)
    expect_equal(raster_extent(full$p), image_extent)
    hidden <- draw(TRUE, clusters = character(), feature = modifyList(gene, list(minCount = Inf)))
    expect_equal(hidden$points, 0L)
    expect_equal(hidden$p$coordinates$limits, fitted$p$coordinates$limits)
    no_image <- draw(TRUE, image_id = "")
    expect_equal(no_image$p$coordinates$limits, fitted$p$coordinates$limits)
    expect_equal(draw(FALSE, image_id = "")$p$coordinates$limits, fitted$p$coordinates$limits)
    for (fit in c(TRUE, FALSE)) {
        no_histology <- draw(fit, spatial = FALSE)
        expect_equal(no_histology$p$coordinates$limits, fitted$p$coordinates$limits)
        expect_length(no_histology$p$layers, 1L)
    }
})

test_that("side-by-side panels share fitted limits for either Y orientation", {
    f <- app_fixture()
    sample <- unique(f$spe$sample_id)[1]
    catalog <- app_image_catalog(f$spe)
    original <- app_static_coord
    calls <- list()
    testthat::local_mocked_bindings(app_static_coord = function(extent, y_down) {
        calls[[length(calls) + 1L]] <<- list(extent = extent, y_down = y_down)
        original(extent, y_down)
    }, .package = "spatialLIBD")
    for (direction in c("up", "down")) {
        calls <- list()
        catalog[[sample]]$sources$lowres$y_direction <- direction
        result <- app_static_spatial(f$spe, sample, "app_cluster",
            list(geneid = rownames(f$spe)[1], assayname = "logcounts",
                minCount = -Inf, cap_percentile = 1, multi_gene_method = "z_score"),
            NULL, "continuous", list(continuous = viridisLite::viridis(21)),
            catalog, "lowres", TRUE, 2, 2, 1, TRUE)
        expect_length(calls, 2L)
        expect_identical(calls[[1]], calls[[2]])
        expect_identical(calls[[1]]$y_down, direction == "down")
        expect_s3_class(result$p, "ggplot")
    }
})
