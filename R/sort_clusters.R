#' Sort clusters by frequency
#'
#' This function takes a vector with cluster labels, recasts it as a `factor()`,
#' and sorts the `factor()` levels by frequency such that the most frequent
#' cluster is the first level and so on.
#'
#' @param clusters A vector with cluster labels.
#' @param map_subset A logical vector of length equal to `clusters` specifying
#' which elements of `clusters` to use to determine the ranking of the clusters.
#'
#' @return A `factor()` version of `clusters` where the levels are ordered by
#' frequency.
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
#' ## We see that we have 10 elements in this vector, which is
#' ## an unnamed character vector
#' clus
#'
#' ## letter 'd' is the most frequent
#' table(clus)
#'
#' ## Sort them and obtain a factor. Notice that it's a named
#' ## factor, and the names correspond to the original values
#' ## in the character vector.
#' sort_clusters(clus)
#'
#' ## Since 'd' was the most frequent, it gets assigned to the first level
#' ## in the factor variable.
#' table(sort_clusters(clus))
#'
#' ## If we skip the first 3 values of clus (which are all 'd'), we can
#' ## change the most frequent cluster. And thus the ordering of the
#' ## factor levels.
#' sort_clusters(clus, map_subset = seq_len(length(clus)) > 3)
#'
#' ## Let's try with a factor variable
#' clus_factor <- factor(clus)
#' ## sort_clusters() returns an identical result in this case
#' stopifnot(identical(sort_clusters(clus), sort_clusters(clus_factor)))
#'
#' ## What happens if you have a logical variable with NAs?
#' set.seed(20240712)
#' log_var <- sample(c(TRUE, FALSE, NA),
#'     1000,
#'     replace = TRUE,
#'     prob = c(0.3, 0.15, 0.55))
#' ## Here, the NAs are the most frequent group.
#' table(log_var, useNA = "ifany")
#'
#' ## The NAs are not used for sorting. Since we have more 'TRUE' than 'FALSE'
#' ## then, 'TRUE' becomes the first level.
#' table(sort_clusters(log_var), useNA = "ifany")
sort_clusters <- function(clusters, map_subset = NULL) {
    if (is.logical(clusters)) {
        clusters <- as.character(clusters)
    }

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
    factor(clusters, levels = names(sort(map)))
}
