#' Create a data frame with required columns of the ve object to use `sce_image_gene_p()` function
#' 
#' Function that creates a data frame containing the colData columns from the ve object and 
#' the array_row and array_col from the spatialCoords slot. This allows to use the function 
#' `sce_image_gene_p()` with ve objects.
#' 
#' @param ve_sub  A
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' created, such as one created by `sce_to_ve()`. It is a subset containing data of just one sample.
#' 
#' @return A `data.frame` containing colData columns from the ve object and 
#' the array_row and array_col from the spatialCoords slot.
#'
#' @export 
#' @family VisiumExperiment-related functions
#' @author Brenda Pardo, Leonardo Collado-Torres


#ve_image_gene_p <- function(ve_sub){
#  cols_to_move <-
#    c(
#      "array_row",
#      "array_col"
#    )
#  df=merge(as.data.frame(SpatialExperiment::spatialCoords(ve_sub)[ ,cols_to_move]), as.data.frame(SummarizedExperiment::colData(ve_sub)))
#  colnames(df)[colnames(df) == "array_row"] <- "imagerow"
#  colnames(df)[colnames(df) == "array_col"] <- "imagecol"
  
  
#  return(df)
#}


ve_image_gene_p <- function(ve_sub){
  cols_to_move <-
    c(
      "Cell_ID",
      "sample_name",
      "in_tissue",
      "array_row",
      "array_col",
      "pxl_col_in_fullres",
      "pxl_row_in_fullres"
    )
  
  df=merge(as.data.frame(SpatialExperiment::spatialCoords(ve_sub)[ ,cols_to_move]), as.data.frame(SummarizedExperiment::colData(ve_sub)))
  colnames(df)[colnames(df) == "array_row"] <- "row"
  colnames(df)[colnames(df) == "array_col"] <- "col"
  colnames(df)[colnames(df) == "Cell_ID"] <- "barcode"
  colnames(df)[colnames(df) == "in_tissue"] <- "tissue"
  colnames(df)[colnames(df) == "pxl_col_in_fullres"] <- "imagecol"
  colnames(df)[colnames(df) == "pxl_row_in_fullres"] <- "imagerow"
  
  return(df)
}


