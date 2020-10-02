#' Read image
#'
#' Helper function for `geom_plot()` that was needed in order to complete
#' `sce_to_ve()`. It generates the `grid::rasterGrob()` data needed by
#' `geom_plot()` that was previously included in the output from
#' `fetch_data("sce")`.
#'
#' @param ve A
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' created, such as one created by `sce_to_ve()`.
#' @param sample_id A `character(1)` specifying the sample ID to work with.
#'
#' @importFrom readbitmap read.bitmap
#' @importFrom grid rasterGrob
#' @importFrom tibble tibble
#' @export
#' @family VisiumExperiment-related functions
#' @return A `tibble::tible()` with a `grob` column that is a `list()` with a
#' `grid::rasterGrob()` object.
#'  @author
#' Brenda Pardo, Leonardo Collado-Torres
read_image <- function(ve, sample_id = names(imagePaths(ve))[1]) {

    ## Check inputs
    stopifnot(sample_id %in% names(imagePaths(ve)))

    ## Read image
    img <- readbitmap::read.bitmap(imagePaths(ve)[as.character(sample_id)])

    ## Create raster
    grob <- grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

    ## Create tibble for ggplot2
    tibble_image <- tibble::tibble(grob = list(grob))
    return(tibble_image)
}
