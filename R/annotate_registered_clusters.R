#' Annotated spatially-registered clusters
#'
#' Once you have computed the enrichment t-statistics for your sc/snRNA-seq data
#' using `registration_wrapper()` and related functions, you can then use
#' `layer_stat_cor()` and `layer_stat_cor_plot()` to perform the spatial
#' registartion of your sc/snRNA-seq data. This function helps interpret that
#' matrix and assign layer labels to your clusters.
#'
#' If you change the input `modeling_results` to `layer_stat_cor()` then the
#' interpretation of this function could change. For example, maybe you have
#' your own spatially-resolved transcriptomics data that doesn't have to be
#' about DLPFC layers.
#'
#' @param cor_matrix The output of `layer_stat_cor()`.
#' @param confidence_threshold A `numeric(1)` specifying the minimum correlation
#' that a given cluster must have against any of the layers (by default) to
#' be considered as having a 'good' assignment. Otherwise, the confidence will
#' be 'poor' and the final label will have an asterisk.
#' @param cutoff_merge_ratio A `numeric(1)` specifying the threshold for merging
#' or not layer assignments (by default). This is a proportion of the difference
#' between the current correlation and the next highest given the units of the
#' next highest correlation. Defaults to a difference of 25% of the next highest
#' correlation: if the observed difference is lower than this threshold, then we
#' keep merging. Higher values will lead to more layers (by default) being
#' merged.
#'
#' @return A `data.frame` with 3 columns. Your `cluster`s, the `layer_confidence`
#' which depends on `confidence_threshold`, and the `layer_label`.
#' @export
#'
#' @examples
#' example("layer_stat_cor", package = "spatialLIBD")
#'
#' ## Obtain labels
#' annotate_registered_clusters(cor_stats_layer)
#'
#' ## More relaxed merging threshold
#' annotate_registered_clusters(cor_stats_layer, cutoff_merge_ratio = 1)
annotate_registered_clusters <- function(cor_matrix, confidence_threshold = 0.25, cutoff_merge_ratio = 0.25) {
    annotated <- apply(cor_matrix, 1, annotate_registered_cluster, cutoff_merge_ratio = cutoff_merge_ratio)

    if (all(colnames(cor_stats_layer) %in% c("WM", paste0("Layer", seq_len(6))))) {
        ## Simplify names when working with the default data
        annotated <- gsub("ayer", "", annotated)
        annotated <- gsub("\\/L", "\\/", annotated)
        annotated <- gsub("^WM\\/", "WM\\/L", annotated)
    }

    result <- data.frame(
        cluster = names(annotated),
        layer_confidence = ifelse(apply(cor_matrix, 1, max) > confidence_threshold, "good", "poor"),
        layer_label = annotated,
        row.names = NULL
    )
    result$layer_label <- paste0(result$layer_label, ifelse(result$layer_confidence == "good", "", "*"))
    return(result)
}

annotate_registered_cluster <- function(remaining, label = "", current = NULL, cutoff_merge_ratio = 0.25) {
    ## Filter negative correlations
    remaining <- remaining[remaining > 0]

    ## There's nothing else to continue with
    if(length(remaining) == 0) {
        return(label)
    }

    ## Find the next highest correlation
    next_i <- which.max(remaining)
    next_cor <- remaining[next_i]

    if(label == "") {
        ## Initial case when we didn't have a label
        annotate_registered_cluster(
            remaining = remaining[-next_i],
            label = names(next_cor),
            current = next_cor,
            cutoff_merge_ratio = cutoff_merge_ratio
        )
    } else {
        ## Find the difference, then divide by the next correlation
        next_diff_ratio <- (current - next_cor) / next_cor

        if (next_diff_ratio > cutoff_merge_ratio) {
            ## It's above the cutoff, so we don't decide to merge
            ## and are done =)
            return(label)
        } else {
            ## It's below the cutoff, so we need to look at the next one
            annotate_registered_cluster(
                remaining = remaining[-next_i],
                label = paste0(label, "/", names(next_cor)),
                current = next_cor,
                cutoff_merge_ratio = cutoff_merge_ratio
            )
        }
    }
}
