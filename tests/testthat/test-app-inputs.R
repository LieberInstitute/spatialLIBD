test_that("preparation accepts ordinary experiments and preserves assay and cell identity", {
    f <- app_fixture()
    source <- f$source
    source$key <- source$ManualAnnotation <- NULL
    rd <- rowData(source)
    rd$symbol <- rd$gene_name
    rd$gene_name <- rd$gene_search <- NULL
    rowData(source) <- rd
    metadata(source) <- list()
    path <- tempfile("prepared-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    app_prepare(source, path)
    d <- open_app_dataset(path)
    expect_s3_class(d, "spatialLIBD_app_dataset")
    expect_identical(dim(d$spe), dim(source))
    expect_identical(colnames(d$spe), colnames(source))
    expect_identical(rownames(d$spe), rownames(source))
    expect_identical(d$spe$key, paste0(colnames(source), "_", source$sample_id))
    expect_true(all(d$spe$ManualAnnotation == "NA"))
    expect_identical(rowData(d$spe)$gene_name, rd$symbol)
    expect_identical(assayNames(d$spe), assayNames(source))
    for (name in assayNames(source)) {
        expect_equal(as.matrix(assay(d$spe, name)), as.matrix(assay(source, name)), tolerance = 0)
    }
    # Original objects are not modified to add app-specific fields.
    expect_null(source$key)
    expect_false("gene_search" %in% names(rowData(source)))
})

test_that("dense and delayed assay inputs retain zeros, negatives and double precision", {
    m <- matrix(c(0, -1, pi, 1e-12, 3, 4), 2, 3)
    for (a in list(m, DelayedArray::DelayedArray(m))) {
        source <- app_upstream(SpatialExperiment(assays = list(values = a),
            rowData = S4Vectors::DataFrame(gene_id = c("g1", "g2")),
            colData = S4Vectors::DataFrame(sample_id = rep("s", 3)),
            spatialCoords = cbind(x = 1:3, y = 2:4)))
        path <- tempfile("dense-")
        app_prepare(source, path)
        actual <- as.matrix(assay(open_app_dataset(path)$spe))
        expect_identical(unname(actual), m)
        unlink(path, recursive = TRUE)
    }
})

test_that("prepared folders relocate with assays and registered image assets", {
    f <- app_fixture()
    # A valid, fill-only two-level Zarr pyramid needs no materialized pixel chunks.
    pyramid <- tempfile(fileext = ".ome.zarr")
    dir.create(pyramid)
    on.exit(unlink(pyramid, recursive = TRUE), add = TRUE)
    jsonlite::write_json(list(zarr_format = 2L), file.path(pyramid, ".zgroup"), auto_unbox = TRUE)
    jsonlite::write_json(list(multiscales = list(list(version = "0.4",
        axes = list(list(name = "y", type = "space"), list(name = "x", type = "space")),
        datasets = list(list(path = "0"), list(path = "1"))))),
        file.path(pyramid, ".zattrs"), auto_unbox = TRUE)
    for (level in 0:1) {
        dir.create(file.path(pyramid, level))
        jsonlite::write_json(list(zarr_format = 2L, shape = c(4L, 4L) / (level + 1L),
            chunks = c(2L, 2L), dtype = "|u1", compressor = NULL,
            fill_value = 0L, order = "C", filters = NULL),
            file.path(pyramid, level, ".zarray"), auto_unbox = TRUE, null = "null")
    }
    path <- tempfile("relocate-")
    moved <- paste0(path, "-moved")
    on.exit(unlink(c(path, moved), recursive = TRUE), add = TRUE)
    sample <- unique(f$source$sample_id)[1]
    app_prepare(f$source, path, fullres_images = setNames(list(list(
        path = pyramid, type = "ome-zarr", extent = c(0, 0, 100, 120),
        y_direction = "down")), sample))
    expect_true(file.rename(path, moved))
    unlink(pyramid, recursive = TRUE)
    d <- open_app_dataset(moved)
    catalog <- app_image_catalog(d$spe)
    source <- app_image_source(catalog, sample)
    expect_identical(source$type, "ome-zarr")
    expect_equal(source$extent, c(0, 0, 100, 120))
    expect_identical(source$y_direction, "down")
    expect_true(startsWith(source$path, normalizePath(moved)))
    expect_true(file.exists(file.path(source$path, ".zattrs")))
    for (id in unique(f$spe$sample_id)) {
        expect_equal(imgRaster(d$spe, sample_id = id, image_id = "lowres"),
            imgRaster(f$spe, sample_id = id, image_id = "lowres"))
    }
    expect_equal(as.matrix(assay(d$spe)), as.matrix(assay(f$source)), tolerance = 0)
})

test_that("preparation protects existing output and rejects ambiguous cell keys", {
    f <- app_fixture()
    path <- tempfile("existing-")
    dir.create(path)
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    writeLines("preserve", file.path(path, "sentinel"))
    expect_error(prepare_app_inputs(f$source, path), "already exists")
    expect_identical(readLines(file.path(path, "sentinel")), "preserve")
    source <- f$source
    source$key <- rep("duplicate", ncol(source))
    output <- tempfile("invalid-")
    expect_error(prepare_app_inputs(source, output), "unique and nonempty")
    expect_false(file.exists(output))
})

test_that("opening validates format and backing files, and run_app requires prepared inputs", {
    f <- app_fixture()
    path <- tempfile("version-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    app_prepare(f$source, path)
    manifest <- readRDS(file.path(path, "manifest.rds"))
    unsupported <- manifest
    unsupported$version <- 999L
    saveRDS(unsupported, file.path(path, "manifest.rds"))
    expect_error(open_app_dataset(path), "Unsupported")
    saveRDS(manifest, file.path(path, "manifest.rds"))
    d <- open_app_dataset(path)
    opts <- app_test_options(f)
    opts$spe <- NULL
    expect_s3_class(do.call(run_app, c(list(dataset = d), opts)), "shiny.appobj")
    expect_error(run_app(f$source), "prepare_app_inputs")
    unlink(file.path(path, manifest$assays[1]), recursive = TRUE)
    expect_error(open_app_dataset(path))
})

test_that("prepared SFE boundaries preserve multipart cells, holes and coordinate precision", {
    skip_if_not_installed("SpatialFeatureExperiment")
    skip_if_not_installed("sf")
    ring <- function(x, y, size) matrix(c(x, y, x + size, y,
        x + size, y + size, x, y + size, x, y), ncol = 2, byrow = TRUE)
    outer <- ring(0.123456789123, 0, 10)
    hole <- ring(2, 2, 1)[5:1, ]
    part1 <- ring(20, 0, 5)
    part2 <- ring(30, 0, 5)
    geometry <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(outer, hole)),
        sf::st_multipolygon(list(list(part1), list(part2)))))
    source <- app_upstream(SpatialFeatureExperiment::SpatialFeatureExperiment(
        assays = list(counts = Matrix::Matrix(matrix(1:6, 3, 2), sparse = TRUE)),
        colData = S4Vectors::DataFrame(sample_id = c("s", "s"), app_cluster = factor(c("one", "two"))),
        colGeometries = list(cellSeg = geometry)))
    rownames(source) <- paste0("g", 1:3)
    colnames(source) <- c("c1", "c2")
    path <- tempfile("sfe-")
    on.exit(unlink(path, recursive = TRUE), add = TRUE)
    app_prepare(source, path)
    d <- open_app_dataset(path)
    con <- file(metadata(d$spe)$app_boundaries$s, "rb")
    header <- readBin(con, integer(), 8, size = 4, endian = "little")
    rings <- readBin(con, integer(), 5, size = 4, endian = "little")
    polygons <- readBin(con, integer(), 4, size = 4, endian = "little")
    polygon_units <- readBin(con, integer(), 3, size = 4, endian = "little")
    ring_units <- readBin(con, integer(), 4, size = 4, endian = "little")
    xy <- readBin(con, numeric(), 40, size = 8, endian = "little")
    close(con)
    expect_identical(header[2:7], c(1L, 4L, 20L, 3L, 8L, 2L))
    expect_equal(rings, c(0, 5, 10, 15, 20))
    expect_equal(polygons, c(0, 2, 3, 4))
    expect_equal(polygon_units, c(0, 1, 1))
    expect_equal(ring_units, c(0, 0, 1, 1))
    expect_identical(xy, as.vector(t(rbind(outer, hole, part1, part2))))
    with_app({
        payload <- app_viewer_message(session)
        expect_equal(payload$total, 2)
        expect_true(nzchar(payload$dataset$geometry))
    }, options = list(spe = d$spe), inputs = list(sample = "s", geneid = "g1", assayname = "counts",
        reduced_name = "", imageid = "", editImg_imageid = "", grid_samples = "s",
        cluster_subset = c("one", "two")))
})
