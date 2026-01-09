test_that("multiplication works", {
  expect_error(fetch_data(type = "BAD_TYPE"))
})


## test ERC

# ## SRT data
# spe_erc_path <- fetch_data(type = "LFF_spatial_ERC_SRT")
# spe_erc_path <- unzip(spe_erc_path, exdir = tempdir())
# spe_erc <- HDF5Array::loadHDF5SummarizedExperiment(
#   file.path(tempdir(), "spe_ERC_annotated")
# )
# spe_erc
# dim: 30494 122202 
# lobstr::obj_size(spe_erc) 3.20 GB
#
# ## SCE data
# sce_erc_path <- fetch_data(type = "LFF_spatial_ERC_snRNAseq")
# sce_erc_path <- unzip(sce_erc_path, exdir = tempdir())
# sce_erc <- HDF5Array::loadHDF5SummarizedExperiment(
#   file.path(tempdir(), "sce_ERC_subcluster")
# )
# sce_erc
# # dim: 38606 122004 
# # lobstr::obj_size(sce_erc) 258.20 MB
# 
# 
# erc_simple_types <- c("LFF_spatial_ERC_SRT_pseudobulk",
#                       "LFF_spatial_ERC_SRT_modeling_results",
#                       "LFF_spatial_ERC_snRNAseq_pseudobulk_broad",
#                       "LFF_spatial_ERC_snRNAseq_pseudobulk_subcluster",
#                       "LFF_spatial_ERC_snRNAseq_modeling_results_broad",
#                       "LFF_spatial_ERC_snRNAseq_modeling_results_subcluster")
#
# names(erc_simple_types) <- erc_simple_types
# erc_data_test <- purrr::map(erc_simple_types, fetch_data)
# 
# purrr::map_int(erc_data_test, length)
# purrr::map_chr(erc_data_test, class)
# LFF_spatial_ERC_SRT_pseudobulk                 LFF_spatial_ERC_SRT_modeling_results 
# "SpatialExperiment"                                               "list" 
# LFF_spatial_ERC_snRNAseq_pseudobulk_broad       LFF_spatial_ERC_snRNAseq_pseudobulk_subcluster 
# "SingleCellExperiment"                               "SingleCellExperiment" 
# LFF_spatial_ERC_snRNAseq_modeling_results_broad LFF_spatial_ERC_snRNAseq_modeling_results_subcluster 
# "list"                                               "list" 
