library("spatialLIBD")

## Obtain the necessary data
if (!exists("sce")) sce <- fetch_data("sce")


pdf(here::here("data-raw", "logo.pdf"), useDingbats = FALSE)
sce_image_clus(
    sce = sce,
    clustervar = "layer_guess_reordered",
    sampleid = "151673",
    colors = libd_layer_colors,
    ... = " LIBD Layers",
    spatial = FALSE
)
dev.off()
