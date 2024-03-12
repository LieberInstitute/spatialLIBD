#' Load analysis data from a 10x Genomics Visium experiment
#'
#' This function expands [SpatialExperiment::read10xVisium()] by reading
#' analysis outputs from SpaceRanger by 10x Genomics.
#'
#' You might want to use `read10xVisiumWrapper()` instead of using this
#' function directly.
#'
#' @inheritParams read10xVisiumWrapper
#'
#' @return A named `list()` with the information about the clustering and the
#' dimension reduction (projections) from the SpaceRanger output by 10x
#' Genomics.
#' @export
#' @family Utility functions for reading data from SpaceRanger output by 10x
#' Genomics
#'
#' @examples
#' ## See 'Using spatialLIBD with 10x Genomics public datasets' for
#' ## a full example using this function.
#' if (interactive()) {
#'     browseVignettes(package = "spatialLIBD")
#' }
#'
#' ## Note that ?SpatialExperiment::read10xVisium doesn't include all the files
#' ## we need to illustrate read10xVisiumWrapper().
read10xVisiumAnalysis <- function(
        samples = "",
        sample_id = paste0("sample", sprintf("%02d", seq_along(samples)))) {
    # check sample identifiers
    if (is.null(sids <- names(samples))) {
        if (is.null(sids <- sample_id)) {
            stop("'sample_id' mustn't be NULL when 'samples' are unnamed")
        } else if (!is.character(sample_id) &&
            length(unique(sample_id)) != length(samples)) {
            stop("'sample_id' should contain as many unique values as 'samples'")
        }
    } else if (length(unique(sids)) != length(samples)) {
        stop("names of 'samples' should be unique")
    }
    names(samples) <- sids

    analysis_options <- c("analysis", "analysis_csv")
    dir <-
        file.path(rep(samples, each = length(analysis_options)), analysis_options)
    dir <- dir[file.exists(dir)]
    stopifnot(length(dir) == length(samples))

    # current_dir <- dir[1]
    # current_sample <- sids[1]

    clusters_all <-
        do.call(
            rbind,
            mapply(
                function(current_dir, current_sample) {
                    clustering_files <-
                        list.files(
                            current_dir,
                            pattern = "clusters.csv",
                            all.files = TRUE,
                            full.names = TRUE,
                            recursive = TRUE
                        )

                    clusters_list <- lapply(clustering_files, read_barcoded_csv)
                    clusters <-
                        Reduce(
                            function(...) {
                                merge(..., by = "barcode", all = TRUE)
                            },
                            clusters_list
                        )
                    clusters$sample_id <- current_sample
                    return(clusters)
                },
                dir,
                sids,
                SIMPLIFY = FALSE,
                USE.NAMES = FALSE
            )
        )


    projection_all <- mapply(
        function(current_dir, current_sample) {
            projection_files <-
                list.files(
                    current_dir,
                    pattern = "projection.csv",
                    all.files = TRUE,
                    full.names = TRUE,
                    recursive = TRUE
                )

            projection_list <- lapply(projection_files, function(x) {
                res <- read_barcoded_csv(x)
                res$sample_id <- current_sample
                return(res)
            })
            names(projection_list) <-
                paste0("10x_", basename(dirname(dirname(
                    projection_files
                ))))

            return(projection_list)
        },
        dir,
        sids,
        SIMPLIFY = FALSE,
        USE.NAMES = FALSE
    )

    projection_names <-
        unique(unlist(lapply(projection_all, names)))
    projections_combined <-
        lapply(projection_names, function(projection_name) {
            one_projection_list <- lapply(projection_all, "[[", projection_name)
            do.call(rbind, one_projection_list)
        })
    names(projections_combined) <- projection_names


    cluster_cols <-
        which(!colnames(clusters_all) %in% c("barcode", "sample_id"))
    colnames(clusters_all)[cluster_cols] <-
        paste0("10x_", colnames(clusters_all)[cluster_cols])

    return(list(clusters = clusters_all, projections = projections_combined))
}


read_barcoded_csv <- function(x) {
    df <- read.csv(x)
    colnames(df) <- tolower(colnames(df))

    if (colnames(df)[2] == "cluster") {
        colnames(df)[2] <-
            gsub("gene_expression_", "", basename(dirname(x)))
    }
    return(df)
}
