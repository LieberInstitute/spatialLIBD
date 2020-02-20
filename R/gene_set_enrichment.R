#' Title
#'
#' @param gene_list
#' @param fdr_cut
#' @param modeling_results
#' @param model_type
#' @param reverse
#'
#' @return
#' @export
#' @author Andrew E Jaffe, Leonardo Collado-Torres
#'
#' @examples
#'
#' asd_sfari <- utils::read.csv(
#'     system.file(
#'         'extdata',
#'         'SFARI-Gene_genes_01-03-2020release_02-04-2020export.csv',
#'         package = 'spatialLIBD'
#'     ),
#'     as.is = TRUE
#' )
#' asd_sfari_geneList <- list(
#'     Gene_SFARI_all = asd_sfari$ensembl.id,
#'     Gene_SFARI_high = asd_sfari$ensembl.id[asd_sfari$gene.score < 3],
#'     Gene_SFARI_syndromic = asd_sfari$ensembl.id[asd_sfari$syndromic == 1]
#' )
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#'
#' asd_sfari_enrichment <- gene_set_enrichment(
#'     gene_list = asd_sfari_geneList,
#'     modeling_results = ori_modeling_results,
#'     model_type = 'specificity'
#' )
#'
#' asd_sfari_enrichment
#'

gene_set_enrichment <-
    function(gene_list,
        fdr_cut = 0.1,
        modeling_results = fetch_data(type = 'modeling_results'),
        model_type = names(modeling_results)[1],
        reverse = FALSE) {
        model_results <- modeling_results[[model_type]]

        ## Keep only the genes present
        geneList_present <- lapply(gene_list, function(x) {
            x <-  x[!is.na(x)]
            x[x %in% model_results$ensembl]
        })

        tstats <-
            model_results[, grep('[f|t]_stat_', colnames(model_results))]
        colnames(tstats) <-
            gsub('[f|t]_stat_', '', colnames(tstats))

        if (reverse) {
            tstats <- tstats * -1
            colnames(tstats) <-
                sapply(strsplit(colnames(tstats), '-'), function(x)
                    paste(rev(x), collapse = '-'))
        }

        fdrs <-
            model_results[, grep('fdr_', colnames(model_results))]


        enrichTab <-
            do.call(rbind, lapply(seq(along.with = tstats), function(i) {
                layer <- tstats[, i] > 0 & fdrs[, i] < fdr_cut
                enrichList <- lapply(geneList_present, function(g) {
                    tt <-
                        table(
                            Set = factor(model_results$ensembl %in% g, c(FALSE, TRUE)),
                            Layer = factor(layer, c(FALSE, TRUE))
                        )
                    fisher.test(tt)
                })
                o <- data.frame(
                    OR = sapply(enrichList, "[[", "estimate"),
                    Pval = sapply(enrichList, "[[", "p.value"),
                    test = colnames(tstats)[i],
                    stringsAsFactors = FALSE
                )
                o$set <- gsub(".odds ratio", "", rownames(o))
                rownames(o) <- NULL
                return(o)
            }))

        enrichTab$model_type <- model_type
        enrichTab$fdr_cut <- fdr_cut

        enrichTab$P_thresh <- enrichTab$Pval
        enrichTab$P_thresh[which(enrichTab$P_thresh < 2.2e-16)] <- 2.2e-16

        return(enrichTab)

    }
