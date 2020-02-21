# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all()
options("golem.app.prod" = TRUE)

## will download the data automatically
# spatialLIBD::run_app()

## Easier to re-use the data
ori_sce <-
    fetch_data('sce', here::here('data-raw/spatialLIBD_files'))
ori_sce_layer <-
    fetch_data('sce_layer', here::here('data-raw/spatialLIBD_files'))
ori_modeling_results <-
    fetch_data('modeling_results',
        here::here('data-raw/spatialLIBD_files'))

ori_sig_genes <- sig_genes_extract_all(n = nrow(ori_sce_layer), ori_modeling_results)


spatialLIBD::run_app(sce = ori_sce,
    sce_layer = ori_sce_layer,
    modeling_results = ori_modeling_results,
    sig_genes = ori_sig_genes,
) # add parameters here (if any)