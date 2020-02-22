#' Plot the gene set enrichment results
#'
#' This function takes the output of [gene_set_enrichment()] and creates a
#' dotplot visualization of the results.
#'
#' @param enrichment The output of [gene_set_enrichment()].
#'
#' @return A [ggplot2::ggplot()] object visualizing the gene set enrichment
#' odds ratio and p-value results.
#' @export
#' @import ggplot2
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
#' gene_set_enrichment_plot(asd_sfari_enrichment)
#'

gene_set_enrichment_plot <- function(enrichment) {
    ggplot(enrichment, aes(
        x = test,
        y = set,
        size = OR,
        color = -log10(P_thresh)
    )) +
        geom_point() + scale_color_continuous(
            low = "white",
            high = "darkred",
            name = "-log10(p)",
            guide = guide_colorbar(reverse = TRUE)
        ) +
        ylab(NULL) + xlab(NULL) + ggtitle("Gene Set Enrichment") +
        theme_dark() +
        theme(
            text = element_text(size = 20),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.key = element_rect(colour = "transparent", fill = "white")
        ) +
        # guides(size = guide_legend(override.aes = list(color = "white"))) +
        scale_size(range = c(1, 10))
}
