# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all()
options("golem.app.prod" = TRUE)

## will download the data automatically
# spatialLIBD::run_app()

## Easier to re-use the data
# library('spatialLIBD') ## requires Bioconductor version 3.10
## check with BiocManager::version()

## In this case, I'm using my local files instead of downloading them
## by telling fetch_data(destdir) where my local files are stored at.
if (!exists('spe')) spe <-
    fetch_data('spe', here::here('data-raw/spatialLIBD_files'))
if (!exists('sce_layer')) sce_layer <-
    fetch_data('sce_layer', here::here('data-raw/spatialLIBD_files'))
if (!exists('modeling_results')) modeling_results <-
    fetch_data('modeling_results',
        here::here('data-raw/spatialLIBD_files'))

sig_genes <-
    sig_genes_extract_all(n = nrow(sce_layer),
        modeling_results,
        sce_layer = sce_layer)

options(repos = BiocManager::repositories())
spatialLIBD::run_app(
    spe = spe,
    sce_layer = sce_layer,
    modeling_results = modeling_results,
    sig_genes = sig_genes,
) # add parameters here (if any)
