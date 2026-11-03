# Run once from the repository root, not during tests.
# APP_SOURCE_DATA may point to previously fetched <type>.rds files.
suppressPackageStartupMessages(library(spatialLIBD))
types <- c("spatialDLPFC_Visium_example_subset",
           "spatialDLPFC_Visium_pseudobulk", "spatialDLPFC_Visium_modeling_results")
source_dir <- Sys.getenv("APP_SOURCE_DATA")
objects <- lapply(types, function(type) {
    path <- file.path(source_dir, paste0(type, ".rds"))
    if (nzchar(source_dir)) readRDS(path) else fetch_data(type)
})
spe <- objects[[1]]
pb <- objects[[2]]
models <- objects[[3]]
# Match by identity, never assume expression and model row order agree.
genes <- Reduce(intersect, c(list(rownames(spe), rownames(pb)),
                            lapply(models, function(x) x$ensembl)))
genes <- head(genes, 40)
samples <- head(unique(spe$sample_id), 2)
spots <- unlist(lapply(samples, function(id) head(which(spe$sample_id == id), 24)))
spe <- spe[genes, spots]
# Keep original measurements and annotations; only shrink background images.
images <- imgData(spe)
images <- images[images$image_id == "lowres", ]
for (i in seq_len(nrow(images))) {
    raster <- imgRaster(images$data[[i]])
    image <- magick::image_resize(magick::image_read(raster), "128x128")
    small <- as.raster(image)
    images$scaleFactor[i] <- images$scaleFactor[i] * ncol(small) / ncol(raster)
    images$data[[i]] <- SpatialImage(small)
}
imgData(spe) <- images
# Compatibility names used by the app; original cluster labels are preserved.
spe$app_cluster <- factor(spe$BayesSpace_harmony_09)
spe$ManualAnnotation <- rep("NA", ncol(spe))
pb <- pb[genes, ]
pb$app_cluster <- factor(pb$BayesSpace)
pb$spatialLIBD <- pb$app_cluster # sig_genes_extract_all() currently requires it.
models <- lapply(models, function(x) x[match(genes, x$ensembl), , drop = FALSE])
saveRDS(list(spe = spe, pb = pb, models = models),
        "tests/testthat/fixtures/app-spatialDLPFC.rds", compress = "xz")
