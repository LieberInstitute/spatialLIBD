#' Plot the gene set enrichment results with ComplexHeatmap
#'
#' This function takes the output of [gene_set_enrichment()] and creates a
#' ComplexHeatmap visualization of the results. Fill of the heatmap represents
#' the -log10(p-val), Odds-ratios are printed for test that pass specified
#' significance threshold `ORcut`.
#'
#' Includes functionality to plot the size of the input gene sets as barplot
#' annotations.
#'
#' @param enrichment The output of [gene_set_enrichment()].
#' @param xlabs A vector of names in the same order and length as
#' `unique(enrichment$ID)`.
#' @param PThresh A `numeric(1)` specifying the P-value threshold for the
#' maximum value in the `-log10(p)` scale.
#' @param ORcut A `numeric(1)` specifying the P-value threshold for the
#' minimum value in the `-log10(p)` scale for printing the odds ratio values
#' in the cells of the resulting plot. Defaults to 3 or p-val < 0.001.
#' @param enrichOnly A `logical(1)` indicating whether to show only odds ratio
#' values greater than 1.
#' @param mypal A `character` vector with the color palette to use. Colors will
#' be in order from 0 to lowest P-val `max(-log(enrichment$Pval))`. Defaults to
#' white, yellow, red pallet.
#' @param plot_SetSize_bar A `logical(1)` indicating whether to plot SetSize
#' from `enrichment` as an `anno_barplot` at the top of the heatmap.
#' @param gene_list_length Optional named `numeric` vector indicating the length
#' of the `gene_list` used to calculate `enrichment`, if included and
#' `plot_setSize_bar = TRUE` then the top `anno_barplot` will show the `SetSize`
#' and the difference from the length of the input gene_list.
#' @param model_sig_length Optional named `numeric` vector indicating the
#' number of significant genes in `modeling_results` used to calculate
#' `enrichment`. If included `anno_barplot` will be added to rows.
#' @param model_colors named `character` vector of colors. It adds colors to
#' row annotations.
#' @param ... Additional parameters passed to
#' [ComplexHeatmap::Heatmap()][ComplexHeatmap::Heatmap()].
#'
#' @return A ([Heatmap-class][ComplexHeatmap::Heatmap-class]) visualizing the
#' gene set enrichment odds ratio and p-value results.
#' @export
#' @importFrom stats reshape
#' @importFrom circlize colorRamp2
#' @importFrom ComplexHeatmap columnAnnotation rowAnnotation Heatmap anno_barplot
#'
#' @family Gene set enrichment functions
#' @author Andrew E Jaffe, Leonardo Collado-Torres, Louise Huuki-Myers
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
#' asd_safari_geneList <- list(
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
#'     gene_list = asd_safari_geneList,
#'     modeling_results = modeling_results,
#'     model_type = "enrichment"
#' )
#'
#' ## Visualize the gene set enrichment results
#'
#' ## Default plot
#' gene_set_enrichment_plot(
#'     enrichment = asd_sfari_enrichment
#' )
#'
#' ## Use a custom green color palette & use shorter gene set names
#' ## (x-axis labels)
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     mypal = c("white", RColorBrewer::brewer.pal(9, "BuGn"))
#' )
#'
#' ## Add bar plot annotations for SetSize of model genes in the gene_lists
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     plot_SetSize_bar = TRUE
#' )
#'
#' ## Add stacked bar plot annotations showing SetSize and difference from the
#' ## length of the input gene_list
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     plot_SetSize_bar = TRUE,
#'     gene_list_length = lapply(asd_safari_geneList, length)
#' )
#'
#' ## add bar plot annotations for number of enriched genes from layers
#' if (!exists("sce_layer")) sce_layer <- fetch_data(type = "sce_layer")
#' sig_genes <- sig_genes_extract(
#'     modeling_results = modeling_results,
#'     model = "enrichment",
#'     sce_layer = sce_layer,
#'     n = nrow(sce_layer)
#' )
#'
#' sig_genes <- sig_genes[sig_genes$fdr < 0.1, ]
#' n_sig_model <- as.list(table(sig_genes$test))
#'
#' ## add barplot with n significant genes from modeling
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     plot_SetSize_bar = TRUE,
#'     model_sig_length = n_sig_model
#' )
#'
#' ## add color annotations
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     plot_SetSize_bar = TRUE,
#'     model_colors = libd_layer_colors
#' )
#'
#' ## add barplot with n significant genes from modeling filled with model color
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub(".*_", "", unique(asd_sfari_enrichment$ID)),
#'     plot_SetSize_bar = TRUE,
#'     model_sig_length = n_sig_model,
#'     model_colors = libd_layer_colors
#' )
#'
gene_set_enrichment_plot <-
    function(
        enrichment,
        xlabs = unique(enrichment$ID),
        PThresh = 12,
        ORcut = 3,
        enrichOnly = FALSE,
        mypal = c("white", RColorBrewer::brewer.pal(9, "YlOrRd")),
        plot_SetSize_bar = FALSE,
        gene_list_length = NULL,
        model_sig_length = NULL,
        model_colors = NULL,
        ...) {
        ## Re-order and shorten names if they match our data
        if (all(unique(enrichment$test) %in% c("WM", paste0("Layer", seq_len(6))))) {
            enrichment$test <-
                factor(gsub("ayer", "", enrichment$test), levels = rev(c(paste0(
                    "L", seq_len(6)
                ), "WM")))
        }

        ## Check inputs
        stopifnot(is(enrichment, "data.frame"))
        stopifnot(all(c("ID", "test", "OR", "Pval") %in% colnames(enrichment)))
        stopifnot(ORcut <= PThresh)
        stopifnot(length(xlabs) == length(unique(enrichment$ID)))

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

        ## sub xlabs labels
        if (!is.null(gene_list_length)) {
            stopifnot(setequal(names(gene_list_length), unique(enrichment$ID)))
            gene_list_length <- gene_list_length[unique(enrichment$ID)]
            names(gene_list_length) <- xlabs
        }

        for (i in seq(length(xlabs))) {
            enrichment$ID <- gsub(unique(enrichment$ID)[[i]], xlabs[[i]], enrichment$ID)
        }

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
            res <- res[, levels(as.factor(enrichment$test)), drop=FALSE]
            t(res)
        }
        wide_or <- make_wide("OR_char")
        wide_p <- make_wide("log10_P_thresh")

        ## define color pallet
        mypal <- circlize::colorRamp2(
            breaks = seq(0, max(wide_p), length.out = length(mypal)),
            colors = mypal
        )

        ## Add gene count annotations
        enrichment_setsize <- unique(enrichment[, c("ID", "SetSize")])

        ## COL annotations
        if (plot_SetSize_bar) {
            if (!is.null(gene_list_length)) {
                stopifnot(all(colnames(wide_p) %in% names(gene_list_length)))
                enrichment_setsize$SetInput <- unlist(gene_list_length[enrichment_setsize$ID])
                enrichment_setsize$Diff <- enrichment_setsize$SetInput - enrichment_setsize$SetSize
            }

            rownames(enrichment_setsize) <- enrichment_setsize$ID
            enrichment_setsize$ID <- NULL
            enrichment_setsize$SetInput <- NULL ## only plot SetSize + Diff

            col_gene_anno <- ComplexHeatmap::columnAnnotation(
                `SetSize` = ComplexHeatmap::anno_barplot(enrichment_setsize)
            )
        } else {
            col_gene_anno <- NULL
        }

        if (!is.null(model_colors)) {
            ## shorten names if they match HumanPilot data
            if (all(c("WM", paste0("Layer", seq_len(6))) %in% names(model_colors))) {
                names(model_colors) <- gsub("ayer", "", names(model_colors))
            }
        }


        ## ROW annotations
        if (!is.null(model_sig_length)) { ## add row barplot annotation

            ## shorten names if they match HumanPilot data
            if (all(names(model_sig_length) %in% c("WM", paste0("Layer", seq_len(6))))) {
                names(model_sig_length) <- gsub("ayer", "", names(model_sig_length))
            }

            stopifnot(all(rownames(wide_p) %in% names(model_sig_length)))
            model_sig_length <- t(data.frame(model_sig_length))

            if (!is.null(model_colors)) { ## barplot with colors
                row_gene_anno <- ComplexHeatmap::rowAnnotation(
                    `n\nmodel sig` = ComplexHeatmap::anno_barplot(model_sig_length[rownames(wide_p), ],
                        gp = gpar(fill = model_colors[rownames(wide_p)])
                    )
                    # annotation_label = anno_title_row
                )
            } else { ## barplot no colors
                row_gene_anno <- ComplexHeatmap::rowAnnotation(
                    `model sig` = ComplexHeatmap::anno_barplot(model_sig_length[rownames(wide_p), ])
                    # annotation_label = anno_title_row
                )
            }
        } else if (!is.null(model_colors)) { ## only apply color annotation

            stopifnot(all(rownames(wide_p) %in% names(model_colors)))
            model_colors <- model_colors[rownames(wide_p)]

            row_gene_anno <- ComplexHeatmap::rowAnnotation(
                " " = rownames(wide_p),
                col = list(" " = model_colors),
                show_legend = FALSE
            )
        } else {
            row_gene_anno <- NULL
        }

        ComplexHeatmap::Heatmap(wide_p,
            col = mypal,
            name = "-log10(p-val)",
            rect_gp = grid::gpar(col = "black", lwd = 1),
            cluster_rows = FALSE,
            cluster_columns = FALSE,
            right_annotation = row_gene_anno,
            top_annotation = col_gene_anno,
            cell_fun = function(j, i, x, y, width, height, fill) {
                grid::grid.text(wide_or[i, j], x, y, gp = grid::gpar(fontsize = 10))
            },
            ...
        )
    }
