#' Download the Human DLPFC Visium data from LIBD
#'
#' This function downloads from `ExperimentHub` Visium, Visium Spatial
#' Proteogenomics (Visium-SPG), or single nucleus RNA-seq (snRNA-seq) data
#' and results analyzed by LIBD from multiple projects.
#' If `ExperimentHub` is not available, this function will
#' download the files from Dropbox using [BiocFileCache::bfcrpath()] unless the
#' files are present already at `destdir`. Note that `ExperimentHub` and
#' `BiocFileCache` will cache the data and automatically detect if you have
#' previously downloaded it, thus making it the preferred way to interact with
#' the data.
#'
#' @param type A `character(1)` specifying which file you want to download. It
#' can either be: `sce` for the
#' \linkS4class{SingleCellExperiment}
#' object containing the spot-level data that includes the information for
#' visualizing the clusters/genes on top of the Visium histology, `sce_layer`
#' for the
#' \linkS4class{SingleCellExperiment}
#' object containing the layer-level data (pseudo-bulked from the spot-level),
#' or `modeling_results` for the list of tables with the `enrichment`,
#' `pairwise`, and `anova` model results from the layer-level data. It can also
#' be `sce_example` which is a reduced version of `sce` just for example
#' purposes. The initial version of `spatialLIBD` downloaded data only from
#' <https://github.com/LieberInstitute/HumanPilot>. As of BioC version 3.13
#' `spe` downloads a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object.
#' As of version 1.11.6, this function also allows downloading data from the
#' <http://research.libd.org/spatialDLPFC/> project. As of version 1.11.12,
#' data from <https://github.com/LieberInstitute/Visium_SPG_AD> can be
#' downloaded.
#'
#' @param destdir The destination directory to where files will be downloaded
#' to in case the `ExperimentHub` resource is not available. If you already
#' downloaded the files, you can set this to the current path where the files
#' were previously downloaded to avoid re-downloading them.
#' @param eh An `ExperimentHub` object
#' [ExperimentHub-class][ExperimentHub::ExperimentHub-class].
#' @param bfc A `BiocFileCache` object
#' [BiocFileCache-class][BiocFileCache::BiocFileCache-class]. Used when
#' `eh` is not available.
#'
#' @return The requested object: `sce`, `sce_layer`, `ve` or `modeling_results` that
#' you have to assign to an object. If you didn't you can still avoid
#' re-loading the object by using `.Last.value`.
#'
#' @export
#' @import ExperimentHub
#' @importFrom AnnotationHub query
#' @importFrom methods is
#' @details The data was initially prepared by scripts at
#' https://github.com/LieberInstitute/HumanPilot and further refined by
#' https://github.com/LieberInstitute/spatialLIBD/blob/master/inst/scripts/make-data_spatialLIBD.R.
#'
#' @examples
#'
#' ## Download the SingleCellExperiment object
#' ## at the layer-level
#' if (!exists("sce_layer")) sce_layer <- fetch_data("sce_layer")
#'
#' ## Explore the data
#' sce_layer
#'
#' ## How to download and load "spatialDLPFC_snRNAseq"
#' \dontrun{
#' sce_path_zip <- fetch_data("spatialDLPFC_snRNAseq")
#' sce_path <- unzip(sce_path_zip, exdir = tempdir())
#' sce <- HDF5Array::loadHDF5SummarizedExperiment(
#'     file.path(tempdir(), "sce_DLPFC_annotated")
#' )
#' sce
#' #> class: SingleCellExperiment
#' #> dim: 36601 77604
#' #> metadata(3): Samples cell_type_colors cell_type_colors_broad
#' #> assays(2): counts logcounts
#' #> rownames(36601): MIR1302-2HG FAM138A ... AC007325.4 AC007325.2
#' #> rowData names(7): source type ... gene_type binomial_deviance
#' #> colnames(77604): 1_AAACCCAAGTTCTCTT-1 1_AAACCCACAAGGTCTT-1 ... 19_TTTGTTGTCTCATTGT-1 19_TTTGTTGTCTTAAGGC-1
#' #> colData names(32): Sample Barcode ... cellType_layer layer_annotation
#' #> reducedDimNames(4): GLMPCA_approx TSNE UMAP HARMONY
#' #> mainExpName: NULL
#' #> altExpNames(0):
#' lobstr::obj_size(sce)
#' #> 172.28 MB
#' }
fetch_data <-
    function(type = c(
        "sce",
        "sce_layer",
        "modeling_results",
        "sce_example",
        "spe",
        "spatialDLPFC_Visium",
        "spatialDLPFC_Visium_example_subset",
        "spatialDLPFC_Visium_pseudobulk",
        "spatialDLPFC_Visium_modeling_results",
        "spatialDLPFC_Visium_SPG",
        "spatialDLPFC_snRNAseq",
        "Visium_SPG_AD_Visium_wholegenome_spe",
        "Visium_SPG_AD_Visium_targeted_spe",
        "Visium_SPG_AD_Visium_wholegenome_pseudobulk_spe",
        "Visium_SPG_AD_Visium_wholegenome_modeling_results",
        "visiumStitched_brain_spe",
        "visiumStitched_brain_spaceranger",
        "visiumStitched_brain_Fiji_out"
    ),
    destdir = tempdir(),
    eh = ExperimentHub::ExperimentHub(),
    bfc = BiocFileCache::BiocFileCache()) {
        ## Some variables
        sce <-
            sce_layer <- modeling_results <- sce_sub <- spe <- NULL

        ## Choose a type among the valid options
        type <- match.arg(type)

        ## Check inputs
        stopifnot(methods::is(eh, "ExperimentHub"))

        ## Deal with the special case of VisiumExperiment first
        if (type == "spe") {
            spe <- sce_to_spe(fetch_data("sce", destdir = destdir, eh = eh))
            return(spe)
        }

        ## Other pre-BioC 3.12 regular files
        if (type == "sce") {
            if (!enough_ram()) {
                warning(
                    paste(
                        "Your system might not have enough memory available.",
                        "Try with a machine that has more memory",
                        "or use the 'sce_example'."
                    )
                )
            }
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <-
                "Human_Pilot_DLPFC_Visium_spatialLIBD_spot_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/f4wcvtdq428y73p/Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata?dl=1"
        } else if (type == "sce_layer") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_layer_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/bg8xwysh2vnjwvg/Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata?dl=1"
        } else if (type == "modeling_results") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_modeling_results"

            ## While EH is not set-up
            file_name <- "Human_DLPFC_Visium_modeling_results.Rdata"
            url <-
                "https://www.dropbox.com/s/se6rrgb9yhm5gfh/Human_DLPFC_Visium_modeling_results.Rdata?dl=1"
        } else if (type == "sce_example") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_DLPFC_Visium_sce_example"

            ## While EH is not set-up
            file_name <- "sce_sub_for_vignette.Rdata"
            url <-
                "https://www.dropbox.com/s/5ra9o8ku9iyyf70/sce_sub_for_vignette.Rdata?dl=1"
        } else if (type == "spatialDLPFC_Visium") {
            if (!enough_ram(7e+09)) {
                warning(
                    paste(
                        "Your system might not have enough memory available (7GB).",
                        "Try with a machine that has more memory."
                    )
                )
            }
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_spe"

            ## While EH is not set-up
            file_name <-
                "spe_filtered_final_with_clusters_and_deconvolution_results.rds"
            url <-
                "https://www.dropbox.com/s/y2ifv5v8g68papf/spe_filtered_final_with_clusters_and_deconvolution_results.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_example_subset") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD_example_subset"
            hub_title <- "spatialDLPFC_Visium_spe_example_subset"

            ## While EH is not set-up
            file_name <- "spatialDLPFC_spe_subset_example.rds"
            url <-
                "https://www.dropbox.com/s/3jm3kjab9lzaemo/spatialDLPFC_spe_subset_example.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_pseudobulk") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_pseudobulk_spe"

            ## While EH is not set-up
            file_name <-
                "sce_pseudo_BayesSpace_k09.rds"
            url <-
                "https://www.dropbox.com/s/pbti4strsfk1m55/sce_pseudo_BayesSpace_k09.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_modeling_results") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <-
                "modeling_results_BayesSpace_k09.Rdata"
            url <-
                "https://www.dropbox.com/s/srkb2ife75px2yz/modeling_results_BayesSpace_k09.Rdata?dl=1"
        } else if (type == "spatialDLPFC_Visium_SPG") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_SPG_spe"

            ## While EH is not set-up
            file_name <-
                "spe.rds"
            url <-
                "https://www.dropbox.com/s/nbf13dna9ibqfaa/spe.rds?dl=1"
        } else if (type == "spatialDLPFC_snRNAseq") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <-
                "sce_DLPFC_annotated.zip"
            url <-
                "https://www.dropbox.com/s/5919zt00vm1ht8e/sce_DLPFC_annotated.zip?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_wholegenome_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_SPG_AD_spe_wholegenome.Rdata"
            url <-
                "https://www.dropbox.com/s/ng036m63grykdm6/Visium_SPG_AD_spe_wholegenome.Rdata?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_targeted_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_SPG_AD_spe_targeted.Rdata"
            url <-
                "https://www.dropbox.com/s/kda9160awc2h8jq/Visium_SPG_AD_spe_targeted.Rdata?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_wholegenome_pseudobulk_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_pseudo_pathology_wholegenome.rds"
            url <-
                "https://www.dropbox.com/s/p8foxj6t6inb8uf/sce_pseudo_pathology_wholegenome.rds?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_wholegenome_modeling_results") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_IF_AD_modeling_results.Rdata"
            url <-
                "https://www.dropbox.com/s/5plupu8bj5m0kfh/Visium_IF_AD_modeling_results.Rdata?dl=1"
        } else if (type == "visiumStitched_brain_spe") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_spe.rds"
            url <-
                "https://www.dropbox.com/scl/fi/9re464y6qaojx3r94nq5u/visiumStitched_brain_spe.rds?rlkey=nq6a82u23xuu9hohr86oodwdi&dl=1"
        } else if (type == "visiumStitched_brain_spaceranger") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_spaceranger.zip"
            url <-
                "https://www.dropbox.com/scl/fi/5jdoaukvhq3v7lk19228y/visiumStitched_brain_spaceranger.zip?rlkey=bdgjc6mgy1ierdad6h6v5g29c&dl=1"
        } else if (type == "visiumStitched_brain_Fiji_out") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_fiji_out.zip"
            url <-
                "https://www.dropbox.com/scl/fi/bevo52e96f2kdwllf8dkk/visiumStitched_brain_fiji_out.zip?rlkey=ptwal8f5zxakzejwd0oqw0lhj&dl=1"
        }

        file_path <- file.path(destdir, file_name)
        ## Use local data if present
        if (!file.exists(file_path)) {
            q <-
                AnnotationHub::query(eh,
                    pattern = c(tag, hub_title)
                )

            if (length(q) == 1) {
                ## ExperimentHub has the data =)
                res <- q[[1]]
                if (type %in% c("sce", "sce_example")) {
                    res <- .update_sce(res)
                } else if (type == "sce_layer") {
                    res <- .update_sce_layer(res)
                }
                return(res)
            } else {
                ## ExperimentHub backup: download from Dropbox
                file_path <- BiocFileCache::bfcrpath(bfc, url)
            }
        }

        ## Now load the data if possible
        message(Sys.time(), " loading file ", file_path)
        if (grepl(".Rdata", file_path)) {
            load(file_path, verbose = FALSE)
            if (type == "sce") {
                return(.update_sce(sce))
            } else if (type == "sce_layer") {
                return(.update_sce_layer(sce_layer))
            } else if (type == "modeling_results" || type == "spatialDLPFC_Visium_modeling_results" || type == "Visium_SPG_AD_Visium_wholegenome_modeling_results") {
                return(modeling_results)
            } else if (type == "sce_example") {
                return(.update_sce(sce_sub))
            } else if (type == "Visium_SPG_AD_Visium_wholegenome_spe" || type == "Visium_SPG_AD_Visium_targeted_spe") {
                return(spe)
            }
        } else if (grepl(".rds", file_path)) {
            return(readRDS(file_path))
        } else {
            file_path
        }
    }


.update_sce <- function(sce) {
    ## Rename here the default cluster we want to show in the shiny app
    sce$spatialLIBD <- sce$layer_guess_reordered_short

    ## Add ManualAnnotation which was formerly called Layer, then drop Layer
    sce$ManualAnnotation <- sce$Layer
    sce$Layer <- NULL

    return(sce)
}

.update_sce_layer <- function(sce_layer) {
    ## Rename here the default cluster we want to show in the shiny app
    sce_layer$spatialLIBD <- sce_layer$layer_guess_reordered_short

    return(sce_layer)
}
