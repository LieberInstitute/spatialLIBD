#' Locate image files
#'
#' Creates a named `character()` vector that can be helpful for locating
#' image files and used with `add_images()`. This function is not necessary if
#' the image files don't use the `spe$sample_id`.
#'
#' @inheritParams vis_clus
#' @param image_dir A `character(1)` specifying a path to a directory containing
#' image files with the pattern `sampleID_pattern.png`.
#' @param image_pattern A `character(1)` specifying the pattern for the image
#' files.
#'
#' @return A named `character()` vector with the path to images.
#' @export
#' @importFrom stats setNames
#' @family Functions for adding non-standard images
#'
#' @examples
#' \dontrun{
#' locate_images(spe, tempdir(), "testImage")
#' }
locate_images <- function(spe, image_dir, image_pattern) {
    image_paths <- setNames(file.path(image_dir, paste0(unique(spe$sample_id), "_", image_pattern, ".png")), unique(spe$sample_id))
    image_paths[file.exists(image_paths)]
}
