#' Quality Control for Spatial Data
#'
#' This function identify spots in a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] (SPE)
#'  with outlier quality control values: low `sum_umi` or `sum_gene`, or high
#'  `expr_chrM_ratio`, utilizing [scuttle::isOutlier][scuttle::isOutlier]. Also identifies in-tissue
#'  edge spots and distance to the edge for each spot.
#'
#' The initial version of this function lives at
#' <https://github.com/LieberInstitute/Visium_SPG_AD/blob/master/code/07_spot_qc/01_qc_metrics_and_segmentation.R>.
#'
#' @param spe a [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#' object that has `sum_umi`, `sum_gene`, `expr_chrM_ratio`, and `in_tissue`
#' variables in the `colData(spe)`. Note that these are automatically created
#' when you build your `spe` object with `spatialLIBD::read10xVisiumWrapper()`.
#' @param overwrite a `logical(1)` specifying whether to overwrite the 7
#' `colData(spe)` columns that this function creates. If set to `FALSE` and any
#' of them are present, the function will return an error.
#'
#' @return A [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#' with added quality control information added to the `colData()`.
#' \describe{
#'    \item{`scran_low_lib_size`}{shows spots that have a low library size.}
#'    \item{`scran_low_n_features`}{spots with a low number of expressed genes.}
#'    \item{`scran_high_Mito_percent`}{spots with a high percent of mitochondrial gene expression.}
#'    \item{`scran_discard`}{spots belonging to either `scran_low_lib_size`,
#' `scran_low_n_feature`, or `scran_high_Mito_percent`.}
#'    \item{`edge_spot`}{spots that are automatically detected as the edge spots
#'    of the `in_tissue` section.}
#'    \item{`edge_distance`}{closest distance in number of spots to either the
#' vertical or horizontal edge.}
#'    \item{`scran_low_lib_size_edge`}{spots that have a low library size and
#'    are an edge spot.}
#' }
#'
#' @export
#' @importFrom dplyr group_by summarize left_join select mutate
#' @importFrom SummarizedExperiment colData
#' @importFrom scuttle isOutlier
#' @author Louise A. Huuki-Myers
#'
#' @examples
#' ## Obtain the necessary data
#' spe_pre_qc <- fetch_data("spatialDLPFC_Visium_example_subset")
#'
#' ## For now, we fake out tissue spots in example data
#' spe_qc <- spe_pre_qc
#' spe_qc$in_tissue[spe_qc$array_col < 10] <- FALSE
#'
#' ## adds QC metrics to colData of the spe
#' spe_qc <- add_qc_metrics(spe_qc, overwrite = TRUE)
#' vars <- colnames(colData(spe_qc))
#' vars[grep("^(scran|edge)", vars)]
#'
#' ## visualize edge spots
#' vis_clus(spe_qc, sampleid = "Br6432_ant", clustervar = "edge_spot")
#'
#' ## specify your own colors
#' vis_clus(
#'     spe_qc,
#'     sampleid = "Br6432_ant",
#'     clustervar = "edge_spot",
#'     colors = c(
#'         "TRUE" = "lightgreen",
#'         "FALSE" = "pink",
#'         "NA" = "red"
#'     )
#' )
#' vis_gene(spe_qc, sampleid = "Br6432_ant", geneid = "edge_distance", minCount = -1)
#'
#' ## Visualize scran QC flags
#'
#' ## Check the spots with low library size as detected by scran::isOutlier()
#' vis_clus(spe_qc, sample_id = "Br6432_ant", clustervar = "scran_low_lib_size")
#'
#' ## Violin plot of library size with low library size highlighted in a
#' ## different color.
#' scater::plotColData(spe_qc[, spe_qc$in_tissue], x = "sample_id", y = "sum_umi", colour_by = "scran_low_lib_size")
#'
#' ## Check any spots that scran::isOutlier() flagged
#' vis_clus(spe_qc, sampleid = "Br6432_ant", clustervar = "scran_discard")
#'
#' ## Low library spots that are on the edge of the tissue
#' vis_clus(spe_qc, sampleid = "Br6432_ant", clustervar = "scran_low_lib_size_edge")
#'
#' ## Use `low_library_size` (or other variables) and `edge_distance` as you
#' ## please.
#' spe_qc$our_low_lib_edge <- spe_qc$scran_low_lib_size & spe_qc$edge_distance < 5
#'
#' vis_clus(spe_qc, sample_id = "Br6432_ant", clustervar = "our_low_lib_edge")
#'
#' ## Clean up
#' rm(spe_qc, spe_pre_qc, vars)
#'
add_qc_metrics <- function(spe, overwrite = FALSE) {
    stopifnot("in_tissue" %in% colnames(colData(spe)))
    stopifnot("sum_umi" %in% colnames(colData(spe)))
    stopifnot("sum_gene" %in% colnames(colData(spe)))
    stopifnot("expr_chrM_ratio" %in% colnames(colData(spe)))


    if (!overwrite) {
        new_vars <- c(
            "scran_discard",
            "scran_low_lib_size",
            "scran_low_n_features",
            "scran_high_Mito_percent",
            "edge_spot",
            "edge_distance",
            "scran_low_lib_size_edge"
        )
        present <- new_vars %in% colnames(colData(spe))
        if (any(present)) {
            stop(
                paste(new_vars[present], collapse = ", "),
                " are all present in the colData(spe). If you want to overwrite them use add_qc_metric(overwrite = TRUE).",
                call. = FALSE
            )
        }
    }

    spe$in_tissue <- as.logical(spe$in_tissue)
    spe_in <- spe[, spe$in_tissue]

    ## QC in-tissue spots

    # define variables
    low_lib_size <- low_n_features <- in_tissue <- sample_id <- NULL

    qc_df <- data.frame(
        log2sum = log2(spe_in$sum_umi),
        log2detected = log2(spe_in$sum_gene),
        subsets_Mito_percent = spe_in$expr_chrM_ratio * 100,
        sample_id = spe_in$sample_id
    )

    qcfilter <- data.frame(
        low_lib_size = scater::isOutlier(qc_df$log2sum, type = "lower", log = TRUE, batch = qc_df$sample_id),
        low_n_features = scater::isOutlier(qc_df$log2detected, type = "lower", log = TRUE, batch = qc_df$sample_id),
        high_subsets_Mito_percent = scater::isOutlier(qc_df$subsets_Mito_percent, type = "higher", batch = qc_df$sample_id)
    ) |>
        dplyr::mutate(discard = (low_lib_size | low_n_features) | high_subsets_Mito_percent)

    ## Add qcfilter cols to colData(spe) after factoring
    ## discard
    spe$scran_discard <- NA
    spe$scran_discard[which(spe$in_tissue)] <- qcfilter$discard

    ## low_lib_size
    spe$scran_low_lib_size <- NA
    spe$scran_low_lib_size[which(spe$in_tissue)] <- qcfilter$low_lib_size

    ## low_n_features
    spe$scran_low_n_features <- NA
    spe$scran_low_n_features[which(spe$in_tissue)] <- qcfilter$low_n_features

    ## high mito percent
    spe$scran_high_Mito_percent <- NA
    spe$scran_high_Mito_percent[which(spe$in_tissue)] <-
        qcfilter$high_subsets_Mito_percent

    ## Find edge spots
    # define variables
    array_row <- array_col <- edge_row <- edge_col <- row_distance <- NULL
    col_distance <- high_subsets_Mito_percent <- NULL

    spot_coords <- colData(spe_in) |>
        as.data.frame() |>
        select(in_tissue, sample_id, array_row, array_col) |>
        group_by(sample_id, array_row) |>
        mutate(
            edge_col = array_col == min(array_col) | array_col == max(array_col),
            col_distance = pmin(
                abs(array_col - min(array_col)),
                abs(array_col - max(array_col))
            )
        ) |>
        group_by(sample_id, array_col) |>
        mutate(
            edge_row = array_row == min(array_row) | array_row == max(array_row),
            row_distance = pmin(
                abs(array_row - min(array_row)),
                abs(array_row - max(array_row))
            )
        ) |>
        group_by(sample_id) |>
        mutate(
            edge_spot = edge_row | edge_col,
            edge_distance = pmin(row_distance, col_distance)
        )


    ## Add Edge info to spe
    spe$edge_spot <- NA
    spe$edge_spot[which(spe$in_tissue)] <- spot_coords$edge_spot

    spe$edge_distance <- NA
    spe$edge_distance[which(spe$in_tissue)] <- spot_coords$edge_distance

    spe$scran_low_lib_size_edge <- NA
    spe$scran_low_lib_size_edge[which(spe$in_tissue)] <- qcfilter$low_lib_size & spot_coords$edge_spot

    return(spe)
}
