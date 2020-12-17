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
usethis::use_package("BiocStyle", "Suggests")
usethis::use_package("RefManageR", "Suggests")
usethis::use_package("ExperimentHub")
usethis::use_package("SingleCellExperiment", "Depends")
usethis::use_package("ggplot2")
usethis::use_package("cowplot")
usethis::use_package("plotly")
usethis::use_package("viridisLite")
usethis::use_package("shinyWidgets")
usethis::use_package("Polychrome")
usethis::use_package("sessioninfo")
usethis::use_package("grid")
usethis::use_package("grDevices")
usethis::use_package("methods")
usethis::use_package("AnnotationHub")
usethis::use_package("utils")
usethis::use_package("png")
usethis::use_package("scater")
usethis::use_package("DT")
usethis::use_package("RColorBrewer")
usethis::use_package("SummarizedExperiment")
usethis::use_package("stats")
usethis::use_package("graphics")
usethis::use_package("S4Vectors")
usethis::use_package("IRanges")
usethis::use_package("fields")
usethis::use_package("here", "Suggests")
usethis::use_package("BiocManager", "Suggests")

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

usethis::use_r("geom_spatial")
usethis::use_r("get_colors")
usethis::use_r("sce_image_clus")
usethis::use_r("sce_image_clus_p")
usethis::use_r("sce_image_gene")
usethis::use_r("sce_image_gene_p")
usethis::use_r("sort_clusters")
usethis::use_r("sce_image_grid")
usethis::use_r("sce_image_grid_gene")
usethis::use_r("fetch_data")
usethis::use_r("sig_genes_extract")
usethis::use_r("sig_genes_extract_all")
usethis::use_r("layer_boxplot")
usethis::use_r("gene_set_enrichment")
usethis::use_r("gene_set_enrichment_plot")
usethis::use_r("layer_stat_cor")
usethis::use_r("layer_stat_cor_plot")
usethis::use_r("data")
usethis::use_r("layer_matrix_plot")
usethis::use_r("check_sce")
usethis::use_r("check_sce_layer")
usethis::use_r("check_modeling_results")
usethis::use_r("check_image_path")

## 2.7 Create files for ExperimentHub

dir.create("inst/extdata")
dir.create("inst/scripts")
edit_file("inst/scripts/make-data_spatialLIBD.R")
edit_file("inst/scripts/make-metadata_spatialLIBD.R")


## 2.8 Add low quality images

sce <-
    fetch_data(
        type = "sce",
        destdir = here::here("data-raw/spatialLIBD_files")
    )
sapply(unique(sce$sample_name), function(x) {
    dir.create(file.path("inst/app/www/data/", x), recursive = TRUE)
})
sapply(unique(sce$sample_name), function(x) {
    system(
        paste0(
            "scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/",
            x,
            "/tissue_lowres_image.png inst/app/www/data/",
            x,
            "/"
        )
    )
})

## 2.9 Misc
system(
    "scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/Layer_Guesses/gene_sets/SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv inst/extdata/"
)


## T-stats from comparing vs snRNA-seq data from Matt Nguyen et al (LIBD)
## Run on JHPCE
## Extract data from https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/dlpfc_snRNAseq_annotation.R

library("SingleCellExperiment")
load(
    here::here(
        "Analysis",
        "Layer_Guesses",
        "rda",
        "dlpfc_snRNAseq_pseudobulked_specific_Ts.Rdata"
    ),
    verbose = TRUE
)
load(
    here::here(
        "Analysis",
        "Layer_Guesses",
        "rda",
        "dlpfc_snRNAseq_pseudobulked.Rdata"
    ),
    verbose = TRUE
)
t0_contrasts_cell <- sapply(eb0_list_cell, function(x) {
    x$t[, 2, drop = FALSE]
})
rownames(t0_contrasts_cell) <- rowData(sce_pseudobulk)$ID
head(t0_contrasts_cell)

## Fix the column names and order
ct <- colData(sce_pseudobulk)
ct <- ct[!duplicated(sce_pseudobulk$prelimCluster), ]
ct <- ct[order(ct$collapsedCluster, ct$prelimCluster), ]
ct$lab <- paste0(ct$prelimCluster, " (", ct$collapsedCluster, ")")

t0_contrasts_cell <-
    t0_contrasts_cell[, as.character(ct$prelimCluster)]
colnames(t0_contrasts_cell) <- ct$lab

pryr::object_size(t0_contrasts_cell)
# 10.7 MB

## Subset further for illustration purposes
load(here::here("Analysis", "Layer_Guesses", "rda", "modeling_results.Rdata"),
    verbose = TRUE
)

tstats <-
    results_specificity[, grep("[f|t]_stat_", colnames(results_specificity))]
pvals <-
    results_specificity[, grep("p_value_", colnames(results_specificity))]

layer_specific_indices <- mapply(
    function(t, p) {
        oo <- order(t, decreasing = TRUE)[seq_len(100)]
    },
    as.data.frame(tstats),
    as.data.frame(pvals)
)
layer_ind <- unique(as.numeric(layer_specific_indices))


tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer <-
    t0_contrasts_cell[results_specificity$ensembl[layer_ind], ]
pryr::object_size(tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer)
# 224 kB
save(
    tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
    file = here::here(
        "Analysis",
        "Layer_Guesses",
        "rda",
        "tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.Rdata"
    )
)

## Save the big one too just in case
tstats_Human_DLPFC_snRNAseq_Nguyen <- t0_contrasts_cell
save(
    tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer,
    file = here::here(
        "Analysis",
        "Layer_Guesses",
        "rda",
        "tstats_Human_DLPFC_snRNAseq_Nguyen.Rdata"
    )
)

## Then copy to data-raw (outside of JHPCE)
system("scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/Layer_Guesses/rda/tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer.Rdata data-raw/")
usethis::use_data_raw(name = "tstats_Human_DLPFC_snRNAseq_Nguyen_topLayer")

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("spatialLIBD")
devtools::build_vignettes()

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
usethis::use_travis(ext = "com")
usethis::use_coverage()
# usethis::use_appveyor()

## 3.3 build a documentation website
usethis::use_pkgdown()
usethis::use_pkgdown_travis()
travis::use_travis_deploy(endpoint = ".com")

# You're now set!
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
