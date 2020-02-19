#' Title
#'
#' @param type
#' @param destdir
#'
#' @return
#' @export
#' @import ExperimentHub
#' @importFrom AnnotationHub query
#' @importFrom methods is
#' @importFrom utils download.file
#'
#' @examples
#'
#' ## Download the SingleCellExperiment object
#' ## at the layer-level
#' sce_layer <- fetch_data('sce_layer')
#' sce_layer
#'
#'
fetch_data <- function(type = 'sce',
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
