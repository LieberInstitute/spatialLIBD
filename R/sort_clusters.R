#' Sort clusters by frequency
#'
#' This function takes a vector with cluster labels and sorts it by frequency
#' such that the most frequent cluster is the first one and so on.
#'
#' @param clusters A vector with cluster labels.
#' @param map_subset A logical vector of length equal to `clusters` specifying
#' which elements of `clusters` to use to determine the ranking of the clusters.
#'
#' @return A factor of length equal to `clusters` where the levels are the new
#' ordered clusters and the names of the factor are the original values from
#' `clusters`.
#'
#' @export
#'
#' @examples
#'
#' ## Build an initial set of cluster labels
#' clus <- letters[unlist(lapply(4:1, function(x) rep(x, x)))]
#'
#' ## In this case, it's a character vector
#' class(clus)
#'
#' ## Sort them and obtain a factor
#' sort_clusters(clus)
sort_clusters <- function(clusters, map_subset = NULL) {
    if (is.null(map_subset)) {
        map_subset <- rep(TRUE, length(clusters))
    } else {
        if (length(map_subset) != length(clusters)) {
            stop(
                "The objects 'clusters' and 'map_subset' do not have the same lengths.",
                call. = FALSE
            )
        }
    }
    map <-
        rank(length(clusters[map_subset]) - table(clusters[map_subset]), ties.method = "first")
    res <- map[clusters]
    factor(res)
}
