
#' Quality Control for Spatial Data
#'
#' @param spe a [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#'
#' @return
#' @export
#'
#' @examples
#'  if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'     
#'     
#'     ## adds QC metrics to colData of the spe
#'     spe_qc <- metrics_qc(spe)
#'     colData(spe_qc)
#'     
#'     table(spe_qc$edge_spot, spe_qc$scran_low_lib_size_edge)
#'     
#'     ## visualize edge spots
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "edge_spot")
#'     vis_gene(spe_qc, sample_id = "151507", geneid = "edge_distance", minCount = -1)
#'     
#'     ## visualize scran QC flags
#'     
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "scran_low_lib_size")
#'     
#'     scater::plotColData(spe_qc, x = "sample_id", y = "sum_umi", colour_by = "scran_low_lib_size")
#'     
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "scran_low_n_features")
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "scran_discard")
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "scran_low_lib_size_edge")
#'     
#'     }
#'     #'     
#'  @importFrom dplyr group_by summarize left_join
metrics_qc <- function(spe) {
  
  qc_df <- data.frame(
    log2sum = log2(spe$sum_umi),
    log2detected = log2(spe$sum_gene),
    subsets_Mito_percent = spe$expr_chrM_ratio*100,
    sample_id = spe$sample_id
  )
  
  qcfilter <- data.frame(
    low_lib_size = scater::isOutlier(qc_df$log2sum, type = "lower", log = TRUE, batch = qc_df$sample_id),
    low_n_features = scater::isOutlier(qc_df$log2detected, type = "lower", log = TRUE, batch = qc_df$sample_id),
    high_subsets_Mito_percent = scater::isOutlier(qc_df$subsets_Mito_percent, type = "higher", batch = qc_df$sample_id)
  ) |> mutate(discard  = (low_lib_size | low_n_features) | high_subsets_Mito_percent)
  
  
  ## Add qcfilter cols to colData(spe) after factoring
  spe$scran_discard <-
    factor(qcfilter$discard, levels = c("TRUE", "FALSE"))
  
  spe$scran_low_lib_size <-
    factor(qcfilter$low_lib_size, levels = c("TRUE", "FALSE"))
  
  spe$scran_low_n_features <-
    factor(qcfilter$low_n_features, levels = c("TRUE", "FALSE"))
  
  spe$scran_high_subsets_Mito_percent <-
    factor(qcfilter$high_subsets_Mito_percent, levels = c("TRUE", "FALSE"))
  
  spe$scran_low_lib_size_low_mito <- 
    factor(qcfilter$low_lib_size & 
             qc_df$subsets_Mito_percent < 0.5, levels = c("TRUE", "FALSE"))
  
  ## Find edge spots
  
  ## Find edge spots
  spot_coords <- colData(spe) |>
    as.data.frame() |>
    select(sample_id, array_row, array_col) |>
    group_by(sample_id, array_row) |>
    mutate(edge_col = array_col == min(array_col) | array_col == max(array_col),
           col_distance = pmin(abs(array_col - min(array_col)),
                               abs(array_col - max(array_col)))
           ) |>
    group_by(sample_id, array_col) |>
    mutate(edge_row = array_row == min(array_row) | array_row == max(array_row),
           row_distance = pmin(abs(array_row - min(array_row)),
                               abs(array_row - max(array_row)))
           ) |>
    group_by(sample_id) |>
    mutate(edge_spot = edge_row | edge_col,
           edge_distance = pmin(row_distance, col_distance))
  
  # spots <- data.frame(
  #   row = spe$array_row,
  #   col = spe$array_col,
  #   sample_id = spe$sample_id
  # )
  
  # edge_spots_row <- group_by(spots, sample_id, row) |> dplyr::summarize(min_col = min(col), max_col = max(col))
  # edge_spots_col <- group_by(spots, sample_id, col) |> dplyr::summarize(min_row = min(row), max_row = max(row))
  # 
  # spots <- dplyr::left_join(spots, edge_spots_row) |> dplyr::left_join(edge_spots_col)
  # spots$edge_spots <- with(spots, row == min_row | row == max_row | col == min_col | col == max_col)
  # 
  # spots$row_distance <- with(spots, pmin(abs(row - min_row), abs(row - max_row)))
  # spots$col_distance <- with(spots, pmin(abs(col - min_col), abs(col - max_col)))
  ## spots$edge_distance <- with(spots, sqrt(row_distance^2 + col_distance^2))
  ## The above is from:
  ## sqrt((x_1 - x_2)^2 + (y_1 - y_2)^2)
  ## but it was wrong, here's a case the the smallest distance is on the column:
  ## sqrt(0^2 + col_distance^2) = col_distance
  # spots$edge_distance <- with(spots, pmin(row_distance, col_distance))
  
  
  spe$edge_spot <- factor(spot_coords$edge_spot,levels = c("TRUE", "FALSE"))
  spe$edge_distance <- spot_coords$edge_distance
  
  
  spe$scran_low_lib_size_edge <- factor(qcfilter$low_lib_size & spot_coords$edge_spot, levels = c("TRUE", "FALSE"))
  
  return(spe)
}