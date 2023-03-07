#' Plot the gene set enrichment results
#'
#' This function takes the output of [gene_set_enrichment()] and creates a
#' heatmap visualization of the results.
#'
#' @param enrichment The output of [gene_set_enrichment()].
#' `unique(enrichment$ID)`. Gets passed to [layer_matrix_plot()].
#' @param PThresh A `numeric(1)` specifying the P-value threshold for the
#' maximum value in the `-log10(p)` scale.
#' @param ORcut A `numeric(1)` specifying the P-value threshold for the
#' minimum value in the `-log10(p)` scale for printing the odds ratio values
#' in the cells of the resulting plot.
#' @param enrichOnly A `logical(1)` indicating whether to show only odds ratio
#' values greater than 1.
#' @param layerHeights A `numeric()` vector of length equal to
#' `length(unique(enrichment$test)) + 1` that starts at 0 specifying where
#' to plot the y-axis breaks which can be used for re-creating the length of
#' each brain layer. Gets passed to [layer_matrix_plot()].
#' @param mypal A vector with the color palette to use. Gets passed to
#' [layer_matrix_plot()].
#' @param cex Passed to [layer_matrix_plot()].
#'
#' @return A plot visualizing the gene set enrichment
#' odds ratio and p-value results.
#' @export
#' @importFrom stats reshape
#' @family Gene set enrichment functions
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @seealso layer_matrix_plot
#' @details Check
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/check_clinical_gene_sets.R
#' to see a full script from where this family of functions is derived from.
#'
#' @examples
#'
#' ## Read in the SFARI gene sets included in the package
#' asd_sfari <- utils::read.csv(
#'     system.file(
#'         "extdata",
#'         "SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv",
#'         package = "spatialLIBD"
#'     ),
#'     as.is = TRUE
#' )
#'
#' ## Format them appropriately
#' asd_sfari_geneList <- list(
#'     Gene_SFARI_all = asd_sfari$ensembl.id,
#'     Gene_SFARI_high = asd_sfari$ensembl.id[asd_sfari$gene.score < 3],
#'     Gene_SFARI_syndromic = asd_sfari$ensembl.id[asd_sfari$syndromic == 1]
#' )
#'
#' ## Obtain the necessary data
#' if (!exists("modeling_results")) {
#'     modeling_results <- fetch_data(type = "modeling_results")
#' }
#'
#' ## Compute the gene set enrichment results
#' asd_sfari_enrichment <- gene_set_enrichment(
#'     gene_list = asd_sfari_geneList,
#'     modeling_results = modeling_results,
#'     model_type = "enrichment"
#' )
#'
#' ## Visualize the gene set enrichment results
#' ## with a custom color palette
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     mypal = c(
#'         "white",
#'         grDevices::colorRampPalette(
#'             RColorBrewer::brewer.pal(9, "BuGn")
#'         )(50)
#'     )
#' )
#' layer_gene_count <- get_gene_enrichment_count(model_results = modeling_results)
#'
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     gene_count_col = sfari_gene_count,
#'     gene_count_row = layer_gene_count
#' )
gene_set_enrichment_plot <-
  function(enrichment,
           PThresh = 12,
           ORcut = 3,
           enrichOnly = FALSE,
           gene_count_col = NULL,
           gene_count_row = NULL,
           anno_title_col = NULL,
           anno_title_row = NULL,
           column_order = NULL,
           anno_add = NULL,
           mypal = c(
             "white",
                    grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(50)
           )) {
    ## Re-order and shorten names if they match our data
    # if (all(unique(enrichment$test) %in% c("WM", paste0("Layer", seq_len(6))))) {
    #     enrichment$test <-
    #         factor(gsub("ayer", "", enrichment$test), levels = rev(c(paste0(
    #             "L", seq_len(6)
    #         ), "WM")))
    # }

    ## Check inputs
    stopifnot(is(enrichment, "data.frame"))
    stopifnot(all(c("ID", "test", "OR", "Pval", "SetSize") %in% colnames(enrichment)))
    stopifnot(ORcut <= PThresh)
    # stopifnot(length(xlabs) == length(unique(enrichment$ID)))

    ## Convert to -log10 scale and threshold the pvalues
    enrichment$log10_P_thresh <-
      round(-log10(enrichment$Pval), 2)
    enrichment$log10_P_thresh[which(enrichment$log10_P_thresh > PThresh)] <-
      PThresh

    ## Change some values for the plot
    if (enrichOnly) {
      enrichment$log10_P_thresh[enrichment$OR < 1] <- 0
    }
    enrichment$OR_char <- as.character(round(enrichment$OR, 2))
    enrichment$OR_char[enrichment$log10_P_thresh < ORcut] <- ""

    ## Make into wide matrices
    make_wide <- function(var = "OR_char") {
      res <-
        reshape(
          enrichment,
          idvar = "ID",
          timevar = "test",
          direction = "wide",
          drop = colnames(enrichment)[!colnames(enrichment) %in% c("ID", "test", var)],
          sep = "_mypattern_"
        )[, -1, drop = FALSE]
      colnames(res) <-
        gsub(".*_mypattern_", "", colnames(res))
      rownames(res) <- unique(enrichment$ID)
      res <- res[, levels(as.factor(enrichment$test))]
      t(res)
    }

    ## Define matrix
    wide_or <- make_wide("OR_char")
    wide_p <- make_wide("log10_P_thresh")

    ## Reorder
    if (!is.null(column_order)) {
      stopifnot(setequal(column_order, colnames(wide_or)))
      wide_or <- wide_or[, column_order]
      wide_p <- wide_p[, column_order]
    }

    if (!is.null(anno_add)) {
      stopifnot(setequal(colnames(anno_add), colnames(wide_or)))
      stopifnot(setequal(rownames(anno_add), rownames(wide_or)))

      wide_or[] <- paste0(anno_add[rownames(wide_or), colnames(wide_or)], "\n", wide_or)
    }

    ## define annotations
    stopifnot(setequal(rownames(gene_count_col), colnames(wide_p)))
    stopifnot(setequal(rownames(gene_count_row), rownames(wide_p)))

    ## get column gene counts from SetSize recorded in enrichment
    gene_count_col <- unique(enrichment[,c("ID", "SetSize")])
    rownames(gene_count_col) <- gene_count_col$ID
    gene_count_col$ID <- NULL

    col_gene_anno <- ComplexHeatmap::columnAnnotation(
      `n genes` = ComplexHeatmap::anno_barplot(gene_count_col[colnames(wide_p), ]),
      annotation_label = anno_title_col
    )
    # row_gene_anno <- ComplexHeatmap::rowAnnotation(
    #   `n genes` = ComplexHeatmap::anno_barplot(gene_count_row[rownames(wide_p), ]),
    #   annotation_label = anno_title_row
    # )

    ComplexHeatmap::Heatmap(wide_p,
                            col = mypal,
                            name = "-log10(p-val)",
                            rect_gp = grid::gpar(col = "black", lwd = 1),
                            cluster_rows = FALSE,
                            cluster_columns = FALSE,
                            # right_annotation = row_gene_anno,
                            top_annotation = col_gene_anno,
                            cell_fun = function(j, i, x, y, width, height, fill) {
                              grid::grid.text(wide_or[i, j], x, y, gp = grid::gpar(fontsize = 10))
                            }
    )
  }


get_gene_list_count <- function(gene_list) {
  data.frame(
    row.names = names(gene_list),
    n = purrr::map_int(gene_list, ~ sum(!is.na(.x)))
  )
}


get_gene_enrichment_count <- function(
    model_results = fetch_data(type = "modeling_results"),
    model_type = "enrichment",
    fdr_cut = 0.1,
    bayes_anno = bayes_anno) {
  model_results <- model_results[[model_type]]

  tstats <-
    model_results[, grep("[f|t]_stat_", colnames(model_results))]
  colnames(tstats) <-
    gsub("[f|t]_stat_", "", colnames(tstats))

  fdrs <-
    model_results[, grep("fdr_", colnames(model_results))]

  enrich_count <- sapply(seq(along.with = tstats), function(i) {
    layer <- sum(tstats[, i] > 0 & fdrs[, i] < fdr_cut)
  })
  data.frame(
    row.names = colnames(tstats),
    n = enrich_count
  )
}
