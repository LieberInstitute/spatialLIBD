#' Title
#'
#' @param sce
#' @param sampleid
#' @param geneid
#' @param spatial
#' @param assayname
#' @param minCount
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'

sce_image_clus_gene <-
    function(sce,
        sampleid,
        geneid = "SCGB2A2; ENSG00000110484",
        spatial = TRUE,
        assayname = 'logcounts',
        minCount = 0,
        ...) {
        sce_sub <- sce[, sce$sample_name == sampleid]
        d <- as.data.frame(colData(sce_sub))

        if(geneid %in% colnames(colData(sce_sub))) {
            d$COUNT <- colData(sce_sub)[[geneid]]
        } else {
            d$COUNT <- assays(sce_sub)[[assayname]][which(rowData(sce_sub)$gene_search == geneid),]
        }
        d$COUNT[d$COUNT <= minCount] <- NA
        sce_image_clus_gene_p(
            sce = sce_sub,
            d = d,
            sampleid = sampleid,
            spatial = spatial,
            title = paste(
                sampleid,
                geneid,
                if(!geneid %in% colnames(colData(sce_sub))) assayname,
                paste0('min Count: >', minCount),
                ...,
                sep = " - "
            ),
            assayname = assayname
        )
    }
