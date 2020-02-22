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
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class]
#' object containing the spot-level data that includes the information for
#' visualizing the clusters/genes on top of the Visium histology, `sce_layer`
#' for the
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class]
#' object containing the layer-level data (pseudo-bulked from the spot-level),
#' or `modeling_results` for the list of tables with the `specificity`,
#' `pairwise`, and `anova` model results from the layer-level data.
#'
#' @param destdir The destination directory to where files will be downloaded
#' to in case the `ExperimentHub` resource is not available. If you already
#' downloaded the files, you can set this to the current path where the files
#' were previously downloaded to avoid re-downloading them.
#' @param eh An `ExperimentHub` object
#' [ExperimentHub-class][ExperimentHub::ExperimentHub-class].
#'
#' @return
#' @export
#' @import ExperimentHub
#' @importFrom AnnotationHub query
#' @importFrom methods is
#' @importFrom utils download.file
#' @details The data was initially prepared by scripts at
#' https://github.com/LieberInstitute/HumanPilot and further refined by
#' https://github.com/LieberInstitute/spatialLIBD/blob/master/inst/scripts/make-data_spatialLIBD.R.
#'
#' @examples
#'
#' ## Download the SingleCellExperiment object
#' ## at the layer-level
#' sce_layer <- fetch_data('sce_layer')
#'
#' ## Explore the data
#' sce_layer
#'

fetch_data <- function(type = c('sce', 'sce_layer', 'modeling_results'),
    destdir = tempdir(),
    eh = ExperimentHub::ExperimentHub()) {
    ## Check inputs
    stopifnot(methods::is(eh, 'ExperimentHub'))
    if (!type %in% c('sce', 'sce_layer', 'modeling_results')) {
        stop(
            "Other 'type' values are not supported. Please use either 'sce', 'sce_layer' or 'modeling_results'.",
            call. = FALSE
        )
    }

    if (type == 'sce') {
        hub_title <- 'Human_Pilot_DLPFC_Visium_spatialLIBD_spot_level_SCE'

        ## While EH is not set-up
        file_name <-
            'Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata'
        url <-
            'https://www.dropbox.com/s/o9ra66h3yfdidhc/Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata?dl=1'
    } else if (type == 'sce_layer')  {
        hub_title <- 'Human_Pilot_DLPFC_Visium_spatialLIBD_layer_level_SCE'

        ## While EH is not set-up
        file_name <-
            'Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata'
        url <-
            'https://www.dropbox.com/s/bg8xwysh2vnjwvg/Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata?dl=1'
    } else if (type == 'modeling_results')  {
        hub_title <- 'Human_Pilot_DLPFC_Visium_spatialLIBD_modeling_results'

        ## While EH is not set-up
        file_name <- 'Human_DLPFC_Visium_modeling_results.Rdata'
        url <-
            'https://www.dropbox.com/s/se6rrgb9yhm5gfh/Human_DLPFC_Visium_modeling_results.Rdata?dl=1'
    }

    q <-
        AnnotationHub::query(eh,
            pattern = c('Human_Pilot_DLPFC_Visium_spatialLIBD', hub_title))

    if (length(q) == 0) {
        ## ExperimentHub backup: download from Dropbox
        file_path <- file.path(destdir, file_name)
        if (!file.exists(file_path)) {
            utils::download.file(url, destfile = file_path)
        }

        load(file_path, verbose = TRUE)
        if (type == 'sce') {
            return(sce)
        } else if (type == 'sce_layer') {
            return(sce_layer)
        } else if (type == 'modeling_results') {
            return(modeling_results)
        }
    } else {
        return(q[[1]])
    }
}
