#' Sample spatial cluster visualization workhorse function for Xenium data with
#' centroid based spatailCoords
#'
#' This function visualizes clusters or categorical variables for one given 
#' sample at the cell-level. This is the function that does all the plotting 
#' behind [vis_clus(datatype = "Xenium")]. To visualize gene-level 
#' (or any continuous variable) use [vis_gene_c()].
#'
#' @inheritParams vis_clus
#' @param d A `data.frame()` with the sample-level information. This is
#' typically obtained using `cbind(colData(spe), spatialCoords(spe))`.
#' @param title The title for the plot.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom tibble tibble
#' @importFrom SpatialExperiment imgData scaleFactors
#' @importFrom S4Vectors metadata
#' @importFrom grid rasterGrob unit
#' @family Spatial cluster visualization functions
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe_xenium_test")
#'     
#'     # spe <- readRDS("../Xenium/XeniumIO_test/spe_Xenium_test.rds")
#'     
#'     ## Prepare the data for the plotting function
#'     spe_sub <- spe[, spe$sample_id == "sample1"]
#'    
#'    # summary(spatialCoords(spe_sub)[,"x_centroid"])
#'    # summary(spatialCoords(spe_sub)[,"y_centroid"])
#'    
#'    ## add catagorical variable
#'    spe_sub$x_half <- ifelse(spatialCoords(spe_sub)[,"x_centroid"] < 3088, "left", "right")
#'    table(spe_sub$x_half)
#'
#'    p <- vis_clus_c(
#'        spe = spe_sub,
#'        d = as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE),
#'        clustervar = "x_half",
#'        sampleid = "sample01.1",
#'        #colors = libd_layer_colors,
#'        colors = c(left = "red", right = "blue"),
#'        title = "Xenium test",
#'        point_size = 1,
#'        alpha = 0.5
#'    )
#'     print(p)
#'
#'     ## Clean up
#'     rm(spe_sub)
#' }
vis_clus_c <-
  function(spe,
           d,
           clustervar,
           sampleid = unique(spe$sample_id)[1],
           colors,
           title,
           alpha = NA,
           point_size = 1,
           auto_crop = TRUE,
           na_color = "#CCCCCC40") {
    ## Some variables
    x_centroid <- y_centroid <- key <- NULL
    # stopifnot(all(c("x_centroid", "y_centroid", "key") %in% colnames(d)))
    

    # ## Crop the image if needed
    # if (auto_crop) {
    #   frame_lims <-
    #     frame_limits(spe, sampleid = sampleid, image_id = image_id)
    #   img <-
    #     img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
    #   adjust <-
    #     list(x = frame_lims$x_min, y = frame_lims$y_min)
    # } else {
      adjust <- list(x = 0, y = 0)
    # }
    # 
    p <- ggplot(
      d,
      aes(
        x = x_centroid,
        y = y_centroid,
        fill = factor(!!sym(clustervar)),
        key = key
      )
    )

    p <- p +
      geom_point(
        shape = 21,
        size = point_size,
        stroke = 0,
        colour = "transparent",
        alpha = alpha
      ) +
      coord_fixed(expand = FALSE) +
      scale_fill_manual(values = colors, na.value = na_color) +
      xlab("") + ylab("") +
      labs(fill = NULL) +
      ggtitle(title) +
      theme_set(theme_bw(base_size = 20)) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.box.spacing = unit(0, "pt")
      )
    return(p)
  }
