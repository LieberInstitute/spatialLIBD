#' Sample spatial gene visualization
#'
#' This function visualizes the gene expression stored in `assays(spe)` or any
#' continuous variable stored in `colData(spe)` for one given sample at the
#' spot-level using (by default) the histology information on the background.
#' To visualize clusters (or any discrete variable) use [vis_clus()].
#'
#' @inheritParams vis_clus
#' @param geneid A `character()` specifying the gene ID(s) stored in
#' `rowData(spe)$gene_search` or a continuous variable(s) stored in
#' `colData(spe)` to visualize. For each ID, if `rowData(spe)$gene_search` is
#' missing, then `rownames(spe)` is used to search for the gene ID. When a
#' vector of length > 1 is supplied, the continuous variables are combined
#' according to `multi_gene_method`, producing a single value for each spot.
#' @param assayname The name of the `assays(spe)` to use for extracting the
#' gene expression data. Defaults to `logcounts`.
#' @param minCount A `numeric(1)` specifying the minimum gene expression (or
#' value in the continuous variable) to visualize. Values at or below this
#' threshold will be set to `NA`. Defaults to `0`.
#' @param viridis A `logical(1)` whether to use the color-blind friendly
#' palette from [viridis][viridisLite::viridis()] or the color palette used
#' in the paper that was chosen for contrast when visualizing the data on
#' top of the histology image. One issue is being able to differentiate low
#' values from NA ones due to the purple-ish histology information that is
#' dependent on cell density.
#' @param cont_colors A `character()` vector of colors that supersedes the
#' `viridis` argument.
#' @param multi_gene_method A `character(1)`: either `"pca"`, `"sparsity"`, or
#' `"z_score"`. This parameter controls how multiple continuous variables are
#' combined for visualization, and only applies when `geneid` has length
#' great than 1. `z_score`: to summarize multiple continuous variables, each is
#' normalized to represent a Z-score. The multiple scores are then averaged.
#' `pca`: PCA dimension reduction is conducted on the matrix formed by the
#' continuous variables, and the first PC is then used and multiplied by -1 if
#' needed to have the majority of the values for PC1 to be positive. `sparsity`:
#' the proportion of continuous variables with positive values for each spot is
#' computed. For more details, check the multi gene vignette at
#' <https://research.libd.org/spatialLIBD/articles/multi_gene_plots.html>.
#'
#' @return A [ggplot2][ggplot2::ggplot] object.
#' @export
#' @importFrom SummarizedExperiment assays
#' @importFrom SpatialExperiment spatialCoords
#' @importFrom rlang arg_match
#' @family Spatial gene visualization functions
#' @details This function subsets `spe` to the given sample and prepares the
#' data and title for [vis_gene_p()]. It also adds a caption to the plot.
#'
#' @examples
#'
#' if (enough_ram()) {
#'     ## Obtain the necessary data
#'     if (!exists("spe")) spe <- fetch_data("spe")
#'
#'     ## Valid `geneid` values are those in
#'     head(rowData(spe)$gene_search)
#'     ## or continuous variables stored in colData(spe)
#'     ## or rownames(spe)
#'
#'     ## Visualize a default gene on the non-viridis scale
#'     p1 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         viridis = FALSE
#'     )
#'     print(p1)
#'
#'     ## Use a custom set of colors in the reverse order than usual
#'     p2 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         cont_colors = rev(viridisLite::viridis(21, option = "magma"))
#'     )
#'     print(p2)
#'
#'     ## Turn the alpha to 1, which makes the NA values have a full alpha
#'     p2b <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         cont_colors = rev(viridisLite::viridis(21, option = "magma")),
#'         alpha = 1
#'     )
#'     print(p2b)
#'
#'     ## Turn the alpha to NA, and use an alpha-blended "forestgreen" for
#'     ## the NA values
#'     # https://gist.githubusercontent.com/mages/5339689/raw/2aaa482dfbbecbfcb726525a3d81661f9d802a8e/add.alpha.R
#'     # add.alpha("forestgreen", 0.5)
#'     p2c <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         cont_colors = rev(viridisLite::viridis(21, option = "magma")),
#'         alpha = NA,
#'         na_color = "#228B2280"
#'     )
#'     print(p2c)
#'
#'     ## Visualize a continuous variable, in this case, the ratio of chrM
#'     ## gene expression compared to the total expression at the spot-level
#'     p3 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = "expr_chrM_ratio"
#'     )
#'     print(p3)
#'
#'     ## Visualize a gene using the rownames(spe)
#'     p4 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = rownames(spe)[which(rowData(spe)$gene_name == "MOBP")]
#'     )
#'     print(p4)
#'
#'     ## Repeat without auto-cropping the image
#'     p5 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = rownames(spe)[which(rowData(spe)$gene_name == "MOBP")],
#'         auto_crop = FALSE
#'     )
#'     print(p5)
#'
#'     #    Define several markers for white matter
#'     white_matter_genes <- c(
#'         "ENSG00000197971", "ENSG00000131095", "ENSG00000123560",
#'         "ENSG00000171885"
#'     )
#'
#'     ## Plot all white matter markers at once using the Z-score combination
#'     ## method
#'     p6 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = white_matter_genes,
#'         multi_gene_method = "z_score"
#'     )
#'     print(p6)
#'
#'     ## Plot all white matter markers at once using the sparsity combination
#'     ## method
#'     p7 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = white_matter_genes,
#'         multi_gene_method = "sparsity"
#'     )
#'     print(p7)
#'
#'     ## Plot all white matter markers at once using the PCA combination
#'     ## method
#'     p8 <- vis_gene(
#'         spe = spe,
#'         sampleid = "151507",
#'         geneid = white_matter_genes,
#'         multi_gene_method = "pca"
#'     )
#'     print(p8)
#' }
vis_gene <-
    function(spe,
    sampleid = unique(spe$sample_id)[1],
    geneid = rowData(spe)$gene_search[1],
    spatial = TRUE,
    assayname = "logcounts",
    minCount = 0,
    viridis = TRUE,
    image_id = "lowres",
    alpha = NA,
    cont_colors = if (viridis) viridisLite::viridis(21) else c("aquamarine4", "springgreen", "goldenrod", "red"),
    point_size = 2,
    auto_crop = TRUE,
    na_color = "#CCCCCC40",
    multi_gene_method = c("z_score", "pca", "sparsity"),
    is_stitched = FALSE,
    ...) {
        multi_gene_method <- rlang::arg_match(multi_gene_method)
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

        #   Verify 'assayname'
        if (!(assayname %in% names(assays(spe)))) {
            stop(sprintf("'%s' is not an assay in 'spe'", assayname), call. = FALSE)
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

        #   Verify legitimacy of names in geneid
        geneid_is_valid <- (geneid %in% rowData(spe_sub)$gene_search) |
            (geneid %in% rownames(spe_sub)) |
            (geneid %in% colnames(colData(spe_sub)))
        if (any(!geneid_is_valid)) {
            stop(
                "Could not find the 'geneid'(s) ",
                paste(geneid[!geneid_is_valid], collapse = ", "),
                call. = FALSE
            )
        }

        #   Grab any continuous colData columns and verify they're all numeric
        cont_cols <- colData(spe_sub)[
            , geneid[geneid %in% colnames(colData(spe_sub))],
            drop = FALSE
        ]
        if (!all(sapply(cont_cols, class) %in% c("numeric", "integer"))) {
            stop(
                "'geneid' can not contain non-numeric colData columns.",
                call. = FALSE
            )
        }
        cont_cols <- as.matrix(cont_cols)

        #   Get the integer indices of each gene in the SpatialExperiment, since we
        #   aren't guaranteed that rownames are gene names
        remaining_geneid <- geneid[!(geneid %in% colnames(colData(spe_sub)))]
        valid_gene_indices <- unique(
            c(
                match(remaining_geneid, rowData(spe_sub)$gene_search),
                match(remaining_geneid, rownames(spe_sub))
            )
        )
        valid_gene_indices <- valid_gene_indices[!is.na(valid_gene_indices)]

        #   Grab any genes
        gene_cols <- t(
            as.matrix(assays(spe_sub[valid_gene_indices, ])[[assayname]])
        )

        #   Combine into one matrix where rows are samples and columns are continuous
        #   features
        cont_matrix <- cbind(cont_cols, gene_cols)

        #   Determine plot and legend titles
        if (ncol(cont_matrix) == 1) {
            plot_title <- paste(sampleid, geneid, ...)
            d$COUNT <- cont_matrix[, 1]
            if (!(geneid %in% colnames(colData(spe_sub)))) {
                legend_title <- sprintf("%s\n min > %s", assayname, minCount)
            } else {
                legend_title <- sprintf("min > %s", minCount)
            }
        } else {
            plot_title <- paste(sampleid, ...)
            if (multi_gene_method == "z_score") {
                d$COUNT <- multi_gene_z_score(cont_matrix)
                legend_title <- paste("Z score\n min > ", minCount)
            } else if (multi_gene_method == "sparsity") {
                d$COUNT <- multi_gene_sparsity(cont_matrix)
                legend_title <- paste("Prop. nonzero\n min > ", minCount)
            } else { # must be 'pca'
                d$COUNT <- multi_gene_pca(cont_matrix)
                legend_title <- paste("PC1\n min > ", minCount)
            }
        }
        d$COUNT[d$COUNT <= minCount] <- NA

        p <- vis_gene_p(
            spe = spe_sub,
            d = d,
            sampleid = sampleid,
            spatial = spatial,
            title = plot_title,
            viridis = viridis,
            image_id = image_id,
            alpha = alpha,
            cont_colors = cont_colors,
            point_size = point_size,
            auto_crop = auto_crop,
            na_color = na_color,
            legend_title = legend_title
        )
        return(p)
    }
