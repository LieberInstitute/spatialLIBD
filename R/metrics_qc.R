
#' Quality Control for Spatial Data
#'
#' This function identify spots in a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] (SPE)
#'  with outlier quality control values: low `sum_umi` or `sum_gene`, or high
#'  `expr_chrM_ratio`, utilizing `scran::isOutlier()`. Also identifies in-tissue
#'  edge spots and distance to the edge for each spot.   
#'
#' @param spe a [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#'
#' @return A [SpatialExperiment][SpatialExperiment::SpatialExperiment-class]
#' with added quiality control information added to the colData.
#' 
#' @export
#'
#' @examples
#'  if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'     
#'     ## fake out tissue spots in example data (TODO add pre-qc data)
#'     spe_qc <- spe
#'     spe_qc$in_tissue[spe_qc$array_col < 10] <- FALSE
#'     
#'     ## adds QC metrics to colData of the spe
#'     spe_qc <- metrics_qc(spe)
#'     colData(spe_qc)
#'          
#'     ## visualize edge spots
#'     vis_clus(spe_qc, sampleid = "151509", clustervar = "edge_spot")
#'     vis_clus(spe_qc, sampleid = "151509", clustervar = "in_tissue")
#'     vis_gene(spe_qc, sampleid = "151509", geneid = "edge_distance", minCount = -1)
#'     
#'     ## visualize scran QC flags
#'     
#'     vis_clus(spe_qc, sample_id = "151507", clustervar = "scran_low_lib_size")
#'     
#'     scater::plotColData(spe_qc, x = "sample_id", y = "sum_umi", colour_by = "scran_low_lib_size")
#'     
#'     vis_clus(spe_qc, sampleid = "151507", clustervar = "scran_low_n_features")
#'     vis_clus(spe_qc, sampleid = "151507", clustervar = "scran_discard")
#'     vis_clus(spe_qc, sampleid = "151507", clustervar = "scran_low_lib_size_edge")
#'     
#'     }
#'     #'     
#'  @importFrom dplyr group_by summarize left_join select mutate
#'  @importFrom SummarizedExperiment colData
#'  @importFrom scater isOutlier 
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
  
  
  ## Add Edge info to spe
  spe$edge_spot <- factor(spot_coords$edge_spot,levels = c("TRUE", "FALSE"))
  spe$edge_distance <- spot_coords$edge_distance
  
  
  spe$scran_low_lib_size_edge <- factor(qcfilter$low_lib_size & spot_coords$edge_spot, levels = c("TRUE", "FALSE"))
  
  return(spe)
}