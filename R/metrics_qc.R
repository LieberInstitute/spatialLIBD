
#' Quality Control for Spatial Data
#'
#' @param spe a [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#'
#' @return
#' @export
#'
#' @examples
metrics_qc <- function(spe) {
  
  qc_df <- data.frame(
    log2sum = log2(spe$sum_umi),
    log2detected = log2(spe$sum_gene),
    subsets_Mito_percent = spe$expr_chrM_ratio*100,
    sample_id = spe$sample_id_original
  )
  
  qcfilter <- DataFrame(
    low_lib_size = isOutlier(qc_df$log2sum, type = "lower", log = TRUE, batch = qc_df$sample_id),
    low_n_features = isOutlier(qc_df$log2detected, type = "lower", log = TRUE, batch = qc_df$sample_id),
    high_subsets_Mito_percent = isOutlier(qc_df$subsets_Mito_percent, type = "higher", batch = qc_df$sample_id)
  )
  qcfilter$discard <- (qcfilter$low_lib_size | qcfilter$low_n_features) | qcfilter$high_subsets_Mito_percent
  
  
  spe$scran_low_lib_size_low_mito <- factor(qcfilter$low_lib_size & qc_df$subsets_Mito_percent < 0.5, levels = c("TRUE", "FALSE"))
  
  
  spe$scran_discard <-
    factor(qcfilter$discard, levels = c("TRUE", "FALSE"))
  spe$scran_low_lib_size <-
    factor(qcfilter$low_lib_size, levels = c("TRUE", "FALSE"))
  spe$scran_low_n_features <-
    factor(qcfilter$low_n_features, levels = c("TRUE", "FALSE"))
  spe$scran_high_subsets_Mito_percent <-
    factor(qcfilter$high_subsets_Mito_percent, levels = c("TRUE", "FALSE"))
  
  ## Find edge spots
  spots <- data.frame(
    row = spe$array_row,
    col = spe$array_col,
    sample_id = spe$sample_id_original
  )
  
  edge_spots_row <- group_by(spots, sample_id, row) %>% summarize(min_col = min(col), max_col = max(col))
  edge_spots_col <- group_by(spots, sample_id, col) %>% summarize(min_row = min(row), max_row = max(row))
  
  spots <- left_join(spots, edge_spots_row) %>% left_join(edge_spots_col)
  spots$edge_spots <- with(spots, row == min_row | row == max_row | col == min_col | col == max_col)
  
  spots$row_distance <- with(spots, pmin(abs(row - min_row), abs(row - max_row)))
  spots$col_distance <- with(spots, pmin(abs(col - min_col), abs(col - max_col)))
  ## spots$edge_distance <- with(spots, sqrt(row_distance^2 + col_distance^2))
  ## The above is from:
  ## sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2)
  ## but it was wrong, here's a case the the smallest distance is on the column:
  ## sqrt(0^2 + col_distance^2) = col_distance
  spots$edge_distance <- with(spots, pmin(row_distance, col_distance))
  
  
  spe$edge_spots <- factor(spots$edge_spots, levels = c("TRUE", "FALSE"))
  spe$edge_distance <- spots$edge_distance
  
  
  spe$scran_low_lib_size_edge <- factor(qcfilter$low_lib_size & spots$edge_distance < 1, levels = c("TRUE", "FALSE"))
  
  return(spe)
}