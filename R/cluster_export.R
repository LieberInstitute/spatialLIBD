#' Export a column with cluster results
#'
#' This function creates a `clusters.csv` file similar to the ones created by
#' SpaceRanger at `outs/analysis/clustering` but with the `key` column that
#' combines the `barcode` and the `sample_id`, which is needed when the `spe`
#' object contains data from multiple samples given that the barcodes are
#' duplicated.
#'
#' @inheritParams vis_clus
#' @param cluster_var A `character(1)` with the name of the variable you wish to
#' export.
#' @param cluster_dir A `character(1)` specifying the output directory, similar
#' to the `outs/analysis/clustering` produced by SpaceRanger.
#' @inheritParams add_key
#'
#' @return The path to the exported `clusters.csv` file.
#' @export
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
#' }
cluster_export <- function(spe, cluster_var, cluster_dir = file.path(tempdir(), "exported_clusters"), overwrite = TRUE) {
    stopifnot(cluster_var %in% colnames(colData(spe)))

    spe <- add_key(spe, overwrite = overwrite)

    df <- data.frame(
        key = key,
        cluster = colData(spe)[[cluster_var]]
    )

    outdir <- file.path(cluster_dir, cluster_var)
    dir.create(outdir, recursive = TRUE)
    outfile <- file.path(outdir, "clusters.csv")

    write.csv(df, file = outfile, row.names = FALSE, quote = FALSE)
    return(invisible(outfile))
}
