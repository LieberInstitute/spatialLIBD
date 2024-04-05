#' Evaluate the enrichment for a list of gene sets
#'
#' Using the layer-level (group-level) data, this function evaluates whether
#' list of gene sets (Ensembl gene IDs) are enriched among the significant
#' genes (FDR < 0.1 by default) genes for a given model type result. Test the
#' alternative hypothesis that OR > 1, i.e. that gene set is over-represented in the
#' set of enriched genes. If you want to check depleted genes, change `reverse`
#' to `TRUE`.
#'
#' @param gene_list A named `list` object (could be a `data.frame`) where each
#' element of the list is a character vector of Ensembl gene IDs.
#' @param fdr_cut A `numeric(1)` specifying the FDR cutoff to use for
#' determining significance among the modeling results genes.
#' @inheritParams sig_genes_extract
#'
#' @return A table in long format with the enrichment results using
#' [stats::fisher.test()].
#'
#' @export
#' @importFrom stats fisher.test
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
#' ## Explore the results
#' asd_sfari_enrichment
gene_set_enrichment <-
    function(gene_list,
    fdr_cut = 0.1,
    modeling_results = fetch_data(type = "modeling_results"),
    model_type = names(modeling_results)[1],
    reverse = FALSE) {
        model_results <- modeling_results[[model_type]]

        ## Keep only the genes present
        geneList_present <- lapply(gene_list, function(x) {
            x <- x[!is.na(x)]
            x[x %in% model_results$ensembl]
        })

        ## warn about low power for small geneLists
        geneList_length <- sapply(geneList_present, length)
        min_genes <- 25
        if (any(geneList_length < min_genes)) {
            warning(
                "Gene list with n < ",
                min_genes,
                " may have insufficent power for enrichment analysis: ",
                paste(names(geneList_length)[geneList_length < 200], collapse = " ,")
            )
        }

        tstats <-
            model_results[, grep("[f|t]_stat_", colnames(model_results))]
        colnames(tstats) <-
            gsub("[f|t]_stat_", "", colnames(tstats))

        if (reverse) {
            tstats <- tstats * -1
            if (model_type == "pairwise") {
                colnames(tstats) <-
                    vapply(strsplit(colnames(tstats), "-"), function(x) {
                        paste(rev(x), collapse = "-")
                    }, character(1))
            } else if (model_type == "anova") {
                stop(
                    "reverse = TRUE does not work with model_type = anova since F-statistics cannot have negative values.",
                    call. = FALSE
                )
            }
        }

        fdrs <-
            model_results[, grep("fdr_", colnames(model_results))]


        enrichTab <-
            do.call(rbind, lapply(seq(along.with = tstats), function(i) {
                layer <- tstats[, i] > 0 & fdrs[, i] < fdr_cut
                tabList <- lapply(geneList_present, function(g) {
                    table(
                        Set = factor(model_results$ensembl %in% g, c(FALSE, TRUE)),
                        Layer = factor(layer, c(FALSE, TRUE))
                    )
                })

                enrichList <-
                    lapply(tabList, fisher.test, alternative = "greater")
                o <- data.frame(
                    OR = vapply(enrichList, "[[", numeric(1), "estimate"),
                    Pval = vapply(enrichList, "[[", numeric(1), "p.value"),
                    test = colnames(tstats)[i],
                    NumSig = vapply(tabList, function(x) {
                        x[2, 2]
                    }, integer(1)),
                    SetSize = vapply(geneList_present, length, integer(1)),
                    stringsAsFactors = FALSE
                )
                o$ID <- gsub(".odds ratio", "", rownames(o))
                rownames(o) <- NULL
                return(o)
            }))

        enrichTab$model_type <- model_type
        if (model_type == "enrichment" && reverse) {
            enrichTab$model_type <- "depletion"
        }
        enrichTab$fdr_cut <- fdr_cut

        return(enrichTab)
    }
