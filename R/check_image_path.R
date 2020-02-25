#' Check input image_path
#'
#' This function checks that the `image_path` vector has the appropriate
#' structure. For more details please check the vignette documentation.
#'
#' @inheritParams run_app
#'
#' @return The input object if all checks are passed.
#' @export
#' @importFrom methods is
#' @family Check input functions
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists('ori_sce')) ori_sce <- fetch_data('sce')
#'
#' ## Get the path to the images
#' img_path <- system.file('inst', 'app', 'www', 'data',
#'     package = 'spatialLIBD')
#'
#' ## Check the object
#' check_image_path(img_path, ori_sce)
#'

check_image_path <- function(image_path, sce) {

    ## Check that it's a path
    stopifnot(is.character(image_path))
    stopifnot(length(image_path) == 1)

    ## Check that the figures exist
    stopifnot(all(file.exists(
        file.path(
            image_path,
            unique(sce$sample_name),
            'tissue_lowres_image.png'
        )
    )))

    return(image_path)
}
