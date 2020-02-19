#' Title
#'
#' @param clusters
#' @param map_subset
#'
#' @return
#' @export
#'
#' @examples
#'

sort_clusters <- function(clusters, map_subset = NULL) {
    if (is.null(map_subset)) {
        map_subset <- rep(TRUE, length(clusters))
    }
    map <-
        rank(length(clusters[map_subset]) - table(clusters[map_subset]), ties.method = 'first')
    res <- map[clusters]
    factor(res)
}
