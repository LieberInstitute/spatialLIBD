#' Plot the gene set enrichment results
#'
#' This function takes the output of [gene_set_enrichment()] and creates a
#' dotplot visualization of the results.
#'
#' @param enrichment The output of [gene_set_enrichment()].
#' @param xlabs A vector of names in the same order and length as
#' `unique(enrichment$ID)`.
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
#' each brain layer.
#' @param mypal A vector with the color palette to use.
#'
#' @return A plot visualizing the gene set enrichment
#' odds ratio and p-value results.
#' @export
#' @importFrom fields image.plot
#' @importFrom graphics axis text abline
#' @importFrom stats reshape
#' @family Gene set enrichment functions
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#' @details Check
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/check_clinical_gene_sets.R
#' to see a full script from where this family of functions is derived from.
#'
#' @examples
#'
#' ## Read in the SFARI gene sets included in the package
#' asd_sfari <- utils::read.csv(
#'     system.file(
#'         'extdata',
#'         'SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv',
#'         package = 'spatialLIBD'
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
#' if (!exists('ori_modeling_results'))
#'     ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' ## Compute the gene set enrichment results
#' asd_sfari_enrichment <- gene_set_enrichment(
#'     gene_list = asd_sfari_geneList,
#'     modeling_results = ori_modeling_results,
#'     model_type = 'specificity'
#' )
#'
#' ## Visualize the gene set enrichment results
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub('.*_', '', unique(asd_sfari_enrichment$ID))
#' )
#'
#' ## Specify the layer heights so it resembles more the length of each
#' ## layer in the brain
#' gene_set_enrichment_plot(
#'     asd_sfari_enrichment,
#'     xlabs = gsub('.*_', '', unique(asd_sfari_enrichment$ID)),
#'     layerHeights = c(0, 40, 55, 75, 85, 110, 120, 135)
#' )
#'

gene_set_enrichment_plot <-
    function(enrichment,
        xlabs = unique(enrichment$ID),
        PThresh = 12,
        ORcut = 3,
        enrichOnly = FALSE,
        layerHeights = c(0, seq_len(length(unique(enrichment$test)))) * 15,
        mypal = c("white",
            grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "BuGn"))(50))) {

        ## Re-order and shorten names if they match our data
        if (all(unique(enrichment$test) %in% c('WM', paste0('Layer', seq_len(6))))) {
            enrichment$test <-
                factor(gsub('ayer', '', enrichment$test), levels = rev(c(paste0(
                    'L', seq_len(6)
                ), 'WM')))
        }

        ## Check inputs
        stopifnot(is(enrichment, 'data.frame'))
        stopifnot(all(c('ID', 'test', 'OR', 'Pval') %in% colnames(enrichment)))
        stopifnot(length(layerHeights) == length(unique(enrichment$test)) + 1)
        stopifnot(ORcut <= PThresh)
        stopifnot(length(xlabs) == length(unique(enrichment$ID)))

        ## Convert to -log10 scale and threshold the pvalues
        enrichment$log10_P_thresh <- round(-log10(enrichment$Pval), 2)
        enrichment$log10_P_thresh[which(enrichment$log10_P_thresh > PThresh)] <-
            PThresh

        ## Change some values for the plot
        if (enrichOnly)
            enrichment$log10_P_thresh[enrichment$OR < 1] <- 0
        enrichment$OR_char <- as.character(round(enrichment$OR, 2))
        enrichment$OR_char[enrichment$log10_P_thresh < ORcut] <- ''

        ## Make into wide matrices
        make_wide <- function(var = 'OR_char') {
            res <-
                reshape(
                    enrichment,
                    idvar = 'ID',
                    timevar = 'test',
                    direction = 'wide',
                    drop = colnames(enrichment)[!colnames(enrichment) %in% c('ID', 'test', var)],
                    sep = '_mypattern_'
                )[, -1, drop = FALSE]
            colnames(res) <- gsub('.*_mypattern_', '', colnames(res))
            rownames(res) <- unique(enrichment$ID)
            res <- res[, levels(as.factor(enrichment$test))]
            t(res)
        }
        wide_or <- make_wide('OR_char')
        wide_p <- make_wide('log10_P_thresh')

        ## For the y-axis labels
        midpoint <- function(x)
            x[-length(x)] + diff(x) / 2

        ## Make the plot
        fields::image.plot(
            x = seq(0, ncol(wide_p), by = 1),
            y = layerHeights,
            z = as.matrix(t(wide_p)),
            col = mypal,
            xaxt = "n",
            yaxt = "n",
            xlab = "",
            ylab = ""
        )
        axis(2,
            rownames(wide_or),
            at = midpoint(layerHeights),
            las = 1)
        axis(1, rep("", ncol(wide_p)), at = seq(0.5, ncol(wide_p) - 0.5))
        text(
            x = seq(0.5, ncol(wide_p) - 0.5),
            y = -1 * max(nchar(xlabs)) / 2,
            xlabs,
            xpd = TRUE,
            srt = 45,
            cex = 2,
            adj = 1
        )
        abline(h = layerHeights, v = c(0, seq_len(ncol(wide_p))))
        text(
            x = rep(seq(0.5, ncol(wide_p) - 0.5), each = nrow(wide_p)),
            y = rep(midpoint(layerHeights), ncol(wide_p)),
            as.character(wide_or),
            cex = 1.5,
            font = 2
        )
    }
