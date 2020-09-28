#' Update the VisiumExperiment scaleFactors
#'
#' Given that our data is a custom
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' with 12 images instead of 1, this function is useful for updating the
#' [scaleFactors][SpatialExperiment::scaleFactors] for a given image.
#'
#' @param ve A
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' created with `fetch_data()`.
#' @param sample_id A `character(1)` with the name of sample of interest.
#'
#' @return A
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' with the updated [scaleFactors][SpatialExperiment::scaleFactors] for the
#' given input `sample_id`.
#'
#' @export
#' @importFrom methods is
#' @importFrom SpatialExperiment scaleFactors "scaleFactors<-"
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("ve")) ve <- fetch_data("ve")
#'
#'     ## Check the default scale factors
#'     scaleFactors(ve)[1:4]
#'
#'     ## Replace them for those for the second image and check them
#'     ve <- update_scaleFactors(ve, "151508")
#'     scaleFactors(ve)[1:4]
#'
#'     ## Restore the original ones from the first image
#'     ve <- update_scaleFactors(ve, "151507")
#'     scaleFactors(ve)[1:4]
#' }
update_scaleFactors <- function(ve, sample_id) {
    ## Check our inputs
    stopifnot(is(ve, "SpatialExperiment"))
    stopifnot(
        "Could not find the specified sample_id in the input ve object." =
            sample_id %in% names(imagePaths(ve))
    )

    ## Obtain the factors we created in fetch_data() with our
    ## custom structure
    facs <- scaleFactors(ve)

    ## Access the factors for our selected input image
    sample_facs <- facs$all_images[[sample_id]]

    ## Re-make our custom structure with the sample scale factors
    ## as the main 4 values required by SpatialExperiment
    new_facs <- c(
        sample_facs,
        all_images = list(facs$all_images)
    )

    ## Save our results
    scaleFactors(ve) <- new_facs

    ## Finish
    return(ve)
}
