#' Title
#'
#' @param sce
#' @param var_registration
#' @param var_sample_id
#'
#' @return
#' @export
#'
#' @examples
#' sce <- scuttle::mockSCE()
#' sce$sample_id <- sample(LETTERS[1:5], ncol(sce), replace = TRUE)
#' ages <- rnorm(5, mean = 20, sd = 4)
#' names(ages) <- LETTERS[1:5]
#' sce$age <- ages[sce$sample_id]
#' rowData(sce)$ensembl <- paste0("ENSG", seq_len(nrow(sce)))
#' rowData(sce)$gene_name <- paste0("gene", seq_len(nrow(sce)))
#' # sce$batches <- sample(1:3, ncol(sce), replace=TRUE)
#' # sce_pseudo <- registration_pseudobulk(sce, "Treatment", "sample_id", c("age", "batches"))
#' sce_pseudo <- registration_pseudobulk(sce, "Treatment", "sample_id", c("age"))
#' colData(sce_pseudo)

registration_pseudobulk <-
    function(sce, var_registration, var_sample_id, covars = NULL) {
        stopifnot(var_registration %in% colnames(colData(sce)))
        stopifnot(var_sample_id %in% colnames(colData(sce)))
        stopifnot(all(
            !c("registration_sample_id", "registration_variable") %in% colnames(colData(sce))
        ))

        ## Pseudo-bulk for our current BayesSpace cluster results
        message(Sys.time(), " make pseudobulk object")
        ## I think this needs counts assay
        sce_pseudo <- scuttle::aggregateAcrossCells(sce,
            DataFrame(
                registration_variable = sce[[var_registration]],
                registration_sample_id = sce[[var_sample_id]]
            ))
        colnames(sce_pseudo) <-
            paste0(sce_pseudo$registration_sample_id,
                "_",
                sce_pseudo$registration_variable)

        ## Check that the covariates are present
        if(!is.null(covars)) {
            for(covariate_i in covars) {
                if (sum(is.na(sce_pseudo[[covariate_i]])) == ncol(sce_pseudo)) {
                    stop("Covariate '", covariate_i, "' has all NAs after pseudo-bulking. Might be due to not being a sample-level covariate.", call. = FALSE)
                }
            }
        }


        ###############################
        message(Sys.time(), " drop lowly expressed genes")
        keep_expr <-
            edgeR::filterByExpr(sce_pseudo, group = "registration_variable")
        sce_pseudo <- sce_pseudo[which(keep_expr), ]

        message(Sys.time(), " normalize expression")
        logcounts(sce_pseudo) <-
            edgeR::cpm(edgeR::calcNormFactors(sce_pseudo),
                log = TRUE,
                prior.count = 1)

        return(sce_pseudo)
    }
