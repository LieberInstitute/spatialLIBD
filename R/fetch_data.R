#' Download the Human DLPFC Visium data from LIBD
#'
#' This function downloads from `ExperimentHub` the dorsolateral prefrontal
#' cortex (DLPFC) human Visium data and results analyzed by LIBD. If
#' `ExperimentHub` is not available, it will download the files from Dropbox
#' using [utils::download.file()] unless the files are present already at
#' `destdir`. Note that `ExperimentHub` will cache the data and automatically
#' detect if you have previously downloaded it, thus making it the preferred
#' way to interact with the data.
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
#' purposes. As of BioC version 3.13 `spe` downloads a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object.
#' As of version 1.11.6 this function also allows downloading data from the
#' <http://research.libd.org/spatialDLPFC/> project.
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
fetch_data <-
    function(type = c(
        "sce",
        "sce_layer",
        "modeling_results",
        "sce_example",
        "spe",
        "spatialDLPFC_Visium",
        "spatialDLPFC_Visium_pseudobulk",
        "spatialDLPFC_Visium_modeling_results",
        "spatialDLPFC_Visium_SPG",
        "spatialDLPFC_snRNAseq"
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

            hub_title <-
                "Human_Pilot_DLPFC_Visium_spatialLIBD_spot_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/f4wcvtdq428y73p/Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata?dl=1"
        } else if (type == "sce_layer") {
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_layer_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/bg8xwysh2vnjwvg/Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata?dl=1"
        } else if (type == "modeling_results") {
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_modeling_results"

            ## While EH is not set-up
            file_name <- "Human_DLPFC_Visium_modeling_results.Rdata"
            url <-
                "https://www.dropbox.com/s/se6rrgb9yhm5gfh/Human_DLPFC_Visium_modeling_results.Rdata?dl=1"
        } else if (type == "sce_example") {
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

            hub_title <- "spatialDLPFC_Visium_spe"

            ## While EH is not set-up
            file_name <-
                "spe_filtered_final_with_clusters_and_deconvolution_results.rds"
            url <-
                "https://www.dropbox.com/s/y2ifv5v8g68papf/spe_filtered_final_with_clusters_and_deconvolution_results.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_pseudobulk") {
            hub_title <- "spatialDLPFC_Visium_pseudobulk_spe"

            ## While EH is not set-up
            file_name <-
                "sce_pseudo_BayesSpace_k09.rds"
            url <-
                "https://www.dropbox.com/s/pbti4strsfk1m55/sce_pseudo_BayesSpace_k09.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_modeling_results") {
            hub_title <- "spatialDLPFC_Visium_modeling_results"

            ## While EH is not set-up
            file_name <-
                "modeling_results_BayesSpace_k09.Rdata"
            url <-
                "https://www.dropbox.com/s/srkb2ife75px2yz/modeling_results_BayesSpace_k09.Rdata?dl=1"
        } else if (type == "spatialDLPFC_Visium_SPG") {
            hub_title <- "spatialDLPFC_Visium_SPG_spe"

            ## While EH is not set-up
            file_name <-
                "spe.rds"
            url <-
                "https://www.dropbox.com/s/nbf13dna9ibqfaa/spe.rds?dl=1"
        } else if (type == "spatialDLPFC_snRNAseq") {
            if (!enough_ram(10e+09)) {
                warning(
                    paste(
                        "Your system might not have enough memory available (10GB).",
                        "Try with a machine that has more memory."
                    )
                )
            }

            hub_title <- "spatialDLPFC_snRNAseq"

            ## While EH is not set-up
            file_name <-
                "TBD"
            url <-
                "TBD?dl=1"
        }

        file_path <- file.path(destdir, file_name)
        ## Use local data if present
        if (!file.exists(file_path)) {
            q <-
                AnnotationHub::query(eh,
                    pattern = c("Human_Pilot_DLPFC_Visium_spatialLIBD", hub_title)
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

        ## Now load the data
        message(Sys.time(), " loading file ", file_path)
        if (grepl(".Rdata", file_path)) {
            load(file_path, verbose = FALSE)
            if (type == "sce") {
                return(.update_sce(sce))
            } else if (type == "sce_layer") {
                return(.update_sce_layer(sce_layer))
            } else if (type == "modeling_results" || type == "spatialDLPFC_Visium_modeling_results") {
                return(modeling_results)
            } else if (type == "sce_example") {
                return(.update_sce(sce_sub))
            }
        }
        readRDS(file_path)
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
