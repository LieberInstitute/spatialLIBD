# Building a Prod-Ready, Robust Shiny Application.
#
# Each step is optional.
#

# 2. All along your project

## 2.1 Add modules
##
# golem::add_module( name = "my_first_module" ) # Name of the module
# golem::add_module( name = "my_other_module" ) # Name of the module

## 2.2 Add dependencies

# usethis::use_package( "thinkr" ) # To call each time you need a new package
usethis::use_package('BiocStyle', 'Suggests')
usethis::use_package('knitcitations', 'Suggests')
usethis::use_package('ExperimentHub', 'Depends')
usethis::use_package('SingleCellExperiment', 'Depends')
usethis::use_package('ggplot2')
usethis::use_package('cowplot')
usethis::use_package('plotly')
usethis::use_package('viridisLite')
usethis::use_package('shinyWidgets')
usethis::use_package('Polychrome')
usethis::use_package('sessioninfo')
usethis::use_package('grid')
usethis::use_package('grDevices')
usethis::use_package('methods')
usethis::use_package('AnnotationHub')
usethis::use_package('utils')
usethis::use_package('png')

## 2.3 Add tests

# usethis::use_test( "app" )
usethis::use_testthat()
usethis::use_test("ExperimentHub")

## 2.4 Add a browser button

# golem::browser_button()

## 2.5 Add external files

# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## 2.6 Add functions

usethis::use_r('geom_spatial')
usethis::use_r('get_colors')
usethis::use_r('sce_image_clus')
usethis::use_r('sce_image_clus_p')
usethis::use_r('sce_image_clus_gene')
usethis::use_r('sce_image_clus_gene_p')
usethis::use_r('sort_clusters')
usethis::use_r('sce_image_grid')
usethis::use_r('sce_image_grid_gene')
usethis::use_r('sce_image_grid_by_clus')
usethis::use_r('fetch_data')
usethis::use_r('sig_genes_extract')
usethis::use_r('sig_genes_extract_all')
usethis::use_r('layer_boxplot')

## 2.7 Create files for ExperimentHub

dir.create('inst/extdata')
dir.create('inst/scripts')
edit_file('inst/scripts/make-data_spatialLIBD.R')
edit_file('inst/scripts/make-metadata_spatialLIBD.R')


## 2.8 Add low quality images

sce <-
    fetch_data(type = 'sce',
        destdir = here::here('data-raw/spatialLIBD_files'))
sapply(unique(sce$sample_name), function(x) {
    dir.create(file.path('inst/app/www/data/', x), recursive = TRUE)
})
sapply(unique(sce$sample_name), function(x) {
    system(
        paste0(
            'scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/',
            x,
            '/tissue_lowres_image.png inst/app/www/data/',
            x,
            '/'
        )
    )
})

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("spatialLIBD")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set!
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
