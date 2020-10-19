#' Create a data frame with required columns of the ve object to use `sce_image_gene_p()` function
#' 
#' Function that creates a data frame containing the colData columns from the ve object and 
#' the array_row and array_col from the spatialCoords slot. This allows to use the function 
#' `sce_image_gene_p()` with ve objects.
#' 
#' @param ve  A
#' [VisiumExperiment-class][SpatialExperiment::VisiumExperiment-class] object
#' created, such as one created by `sce_to_ve()`. It is a subset containing data of just one sample.
#' @param meta A `data.frame` containing colData columns from the ve object.
#' 
#' @return A `data.frame` containing colData columns from the ve object and 
#' the pxl_col_in_fullres and pxl_row_in_fullres from the spatialCoords slot renamed as imagecol 
#' and imagerow.
#'
#' @export 
#' @family VisiumExperiment-related functions
#' @author Brenda Pardo, Leonardo Collado-Torres


ve_image_colData <- function(ve, meta=as.data.frame(colData(ve))){
  if(all(c("imagecol", "imagerow")%in% colnames(meta))) return(meta)
  cols_to_move <-
    c(
      "pxl_col_in_fullres",
      "pxl_row_in_fullres"
    )
  
  df=as.data.frame(SpatialExperiment::spatialCoords(ve)[ ,cols_to_move])

  colnames(df)[colnames(df) == "pxl_col_in_fullres"] <- "imagecol"
  colnames(df)[colnames(df) == "pxl_row_in_fullres"] <- "imagerow"
  
  df=cbind(df,meta)
  
  
  return(df)
}


