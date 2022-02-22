#' Import cluster results
#'
#' This function imports previously exported clustering results with
#' `cluster_export()` and adds them to the `colData()` slot of your
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object.
#'
#' @inheritParams cluster_export
#' @param prefix A `character(1)` specifying the prefix to use when naming
#' these new cluster variables.
#' @inheritParams add_key
#'
#' @return A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object
#' with the imported clusters appended on the `colData()`.
#' @export
#'
#' @family cluster export/import utility functions
#'
#' @examples
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Export two cluster variables
#'     cluster_export(spe, "spatialLIBD")
#'     cluster_export(spe, "GraphBased")
#'
#'     ## Re-import them
#'     colData(cluster_import(spe))
#' }
cluster_import <- function(spe, cluster_dir = file.path(tempdir(), "exported_clusters"), prefix = "imported_", overwrite = TRUE) {
    clustering_files <-
        list.files(
            cluster_dir,
            pattern = "clusters.csv",
            all.files = TRUE,
            full.names = TRUE,
            recursive = TRUE
        )

    clusters_list <- lapply(clustering_files, read_barcoded_csv)
    clusters <- Reduce(function(...) merge(..., by = "key", all = TRUE), clusters_list)
    cluster_cols <- which(colnames(clusters) != "key")
    colnames(clusters)[cluster_cols] <- paste0(prefix, colnames(clusters)[cluster_cols])

    spe <- add_key(spe, overwrite = overwrite)

    merged_info <-
        merge(
            colData(spe),
            clusters,
            by = "key",
            sort = FALSE,
            all = TRUE
        )
    m <- match(spe$key, merged_info$key)
    merged_info <- merged_info[m, ]
    spot_names <- rownames(colData(spe))

    colData(spe) <- DataFrame(merged_info, check.names = FALSE)
    colnames(spe) <- spot_names
    return(spe)
}
