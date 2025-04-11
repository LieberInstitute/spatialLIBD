#' Sample image visualization
#'
#' This function visualizes the histology image for selected sample. Matches
#' crop and settings of [vis_clus()] and [vis_gene()].
#'
#' @param spe A
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object. See [fetch_data()] for how to download some example objects or
#' [read10xVisiumWrapper()] to read in `spaceranger --count` output files and
#' build your own `spe` object.
#' @param sampleid A `character(1)` specifying which sample to plot from
#' `colData(spe)$sample_id` (formerly `colData(spe)$sample_name`).
#' @param image_id A `character(1)` with the name of the image ID you want to
#' use in the background.
#' @param is_stitched A \code{logical(1)} vector: If `TRUE`, expects a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] built
#' with `visiumStitched::build_spe()`.
#' <http://research.libd.org/visiumStitched/reference/build_spe.html>; in
#' particular, expects a logical colData column `exclude_overlapping`
#' specifying which spots to exclude from the plot. Sets `auto_crop = FALSE`.
#' @param title_suffix A `character(1)` passed to [paste()][base::paste] to
#' modify the title of the plot following the `sampleid`.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @family Spatial cluster visualization functions
#' @export
#' @importFrom SpatialExperiment spatialCoords
#' @details This function subsets `spe` to the given sample and prepares the
#' data and title for [vis_clus_p()].
#'
#' @examples
#'
#' if (enough_ram()) {
#'   ## Obtain the necessary data
#'   if (!exists("spe")) spe <- fetch_data("spe")
#'
#'   ## Print the "lowres" image for sample 151673
#'   p1 <- vis_image(
#'     spe = spe,
#'     sampleid = "151673"
#'   )
#'   print(p1)
#'
#'   ## Without auto-cropping the image
#'   p2 <- vis_image(
#'     spe = spe,
#'     sampleid = "151673",
#'     auto_crop = FALSE
#'   )
#'   print(p2)
#' }
vis_image <- function(
    spe,
    sampleid = unique(spe$sample_id)[1],
    image_id = "lowres",
    auto_crop = TRUE,
    is_stitched = FALSE,
    title_suffix = NULL) {
  #   Verify existence and legitimacy of 'sampleid'
  if (
    !("sample_id" %in% colnames(colData(spe))) ||
      !(sampleid %in% spe$sample_id)
  ) {
    stop(
      paste(
        "'spe$sample_id' must exist and contain the ID", sampleid
      ),
      call. = FALSE
    )
  }

  #   Check validity of spatial coordinates
  if (!setequal(c("pxl_col_in_fullres", "pxl_row_in_fullres"), colnames(spatialCoords(spe)))) {
    stop(
      "Abnormal spatial coordinates: should have 'pxl_row_in_fullres' and 'pxl_col_in_fullres' columns.",
      call. = FALSE
    )
  }

  spe_sub <- spe[, spe$sample_id == sampleid]

  if (is_stitched) {
    #   Drop excluded spots and calculate an appropriate point size
    temp <- prep_stitched_data(spe_sub, point_size, image_id)
    spe_sub <- temp$spe
    point_size <- temp$point_size

    #   Frame limits are poorly defined for stitched data
    auto_crop <- FALSE
  }

  d <- as.data.frame(cbind(colData(spe_sub), SpatialExperiment::spatialCoords(spe_sub)), optional = TRUE)

  pxl_row_in_fullres <- pxl_col_in_fullres <- key <- NULL

  img <- SpatialExperiment::imgRaster(spe,
    sample_id = sampleid,
    image_id = image_id
  )

  ## Crop the image if needed
  if (auto_crop) {
    frame_lims <-
      frame_limits(spe, sampleid = sampleid, image_id = image_id)
    img <-
      img[frame_lims$y_min:frame_lims$y_max, frame_lims$x_min:frame_lims$x_max]
    adjust <-
      list(x = frame_lims$x_min, y = frame_lims$y_min)
  } else {
    adjust <- list(x = 0, y = 0)
  }

  title <- paste(sampleid, title_suffix)

  grob <- grid::rasterGrob(img,
    width = grid::unit(1, "npc"),
    height = grid::unit(1, "npc")
  )

  p <- ggplot() +
    geom_spatial(
      data = tibble::tibble(grob = list(grob)),
      aes(grob = grob),
      x = 0.5,
      y = 0.5
    ) +
    xlim(0, ncol(img)) +
    ylim(nrow(img), 0) +
    xlab("") +
    ylab("") +
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
