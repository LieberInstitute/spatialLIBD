#' Title
#'
#' @param sce
#' @param sampleid
#' @param geneid
#' @param spatial
#' @param assayname
#' @param minCount
#' @param viridis
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' ori_sce <- fetch_data('sce')
#' sce_image_clus_gene(
#'     sce = ori_sce,
#'     sampleid = '151507'
#' )
#'
#' sce_image_clus_gene(
#'     sce = ori_sce,
#'     sampleid = '151507',
#'     geneid = 'expr_chrM_ratio'
#' )
#'

sce_image_clus_gene <-
    function(sce,
        sampleid,
        geneid = "SCGB2A2; ENSG00000110484",
        spatial = TRUE,
        assayname = 'logcounts',
        minCount = 0,
        viridis = TRUE,
        ...) {
        sce_sub <- sce[, sce$sample_name == sampleid]
        d <- as.data.frame(colData(sce_sub))

        if (geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[geneid]]
        } else {
            d$COUNT <-
                assays(sce_sub)[[assayname]][which(rowData(sce_sub)$gene_search == geneid),]
        }
        d$COUNT[d$COUNT <= minCount] <- NA
        p <- sce_image_clus_gene_p(
            sce = sce_sub,
            d = d,
            sampleid = sampleid,
            spatial = spatial,
            title = paste(sampleid,
                geneid,
                ...),
            viridis = viridis
        )
        p + labs(caption = paste(if (!geneid %in% colnames(colData(sce_sub)))
            assayname
            else
                NULL,
            'min >', minCount))
    }
