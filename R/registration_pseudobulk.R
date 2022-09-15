#' Spatial registration: pseudobulk
#'
#' Pseudo-bulk the gene expression, filter lowly-expressed genes, and normalize.
#' This is the first step for spatial registration and for statistical modeling.
#'
#' @param sce A
#' [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class]
#' object or one that inherits its properties.
#' @param var_registration A `character(1)` specifying the `colData(sce)`
#' variable of interest against which will be used for computing the relevant
#' statistics.
#' @param var_sample_id A `character(1)` specifying the `colData(sce)` variable
#' with the sample ID.
#' @param covars A `character()` with names of sample-level covariates.
#'
#' @return A pseudo-bulked [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class] object.
#' @importFrom SingleCellExperiment logcounts
#' @importFrom scuttle aggregateAcrossCells
#' @importFrom edgeR filterByExpr calcNormFactors
#' @export
#' @family spatial registration and statistical modeling functions
#'
#' @examples
#' ## Ensure reproducibility of example data
#' set.seed(20220907)
#'
#' ## Generate example data
#' sce <- scuttle::mockSCE()
#'
#' ## Add some sample IDs
#' sce$sample_id <- sample(LETTERS[1:5], ncol(sce), replace = TRUE)
#'
#' ## Add a sample-level covariate: age
#' ages <- rnorm(5, mean = 20, sd = 4)
#' names(ages) <- LETTERS[1:5]
#' sce$age <- ages[sce$sample_id]
#'
#' ## Add gene-level information
#' rowData(sce)$ensembl <- paste0("ENSG", seq_len(nrow(sce)))
#' rowData(sce)$gene_name <- paste0("gene", seq_len(nrow(sce)))
#'
#' ## Pseudo-bulk
#' sce_pseudo <- registration_pseudobulk(sce, "Cell_Cycle", "sample_id", c("age"))
#' colData(sce_pseudo)
registration_pseudobulk <-
    function(sce,
    var_registration,
    var_sample_id,
    covars = NULL) {

        ## Check that inputs are correct
        stopifnot(is(sce, "SingleCellExperiment"))
        stopifnot(var_registration %in% colnames(colData(sce)))
        stopifnot(var_sample_id %in% colnames(colData(sce)))
        stopifnot(all(
            !c("registration_sample_id", "registration_variable") %in% colnames(colData(sce))
        ))

        ## Avoid any incorrect inputs that are otherwise hard to detect
        stopifnot(!var_registration %in% covars)
        stopifnot(!var_sample_id %in% covars)
        stopifnot(var_registration != var_sample_id)

        ## Check that the values in the registration variable are ok
        uniq_var_regis <- unique(sce[[var_registration]])
        if (any(grepl("\\+|\\-", uniq_var_regis))) {
            stop(
                "Remove the + and - signs in colData(sce)[, '",
                var_registration,
                "'] to avoid downstream issues.",
                call. = FALSE
            )
        }

        ## Pseudo-bulk for our current BayesSpace cluster results
        message(Sys.time(), " make pseudobulk object")
        ## I think this needs counts assay
        sce_pseudo <- scuttle::aggregateAcrossCells(
            sce,
            DataFrame(
                registration_variable = sce[[var_registration]],
                registration_sample_id = sce[[var_sample_id]]
            )
        )
        colnames(sce_pseudo) <-
            paste0(
                sce_pseudo$registration_sample_id,
                "_",
                sce_pseudo$registration_variable
            )

        ## Check that the covariates are present
        if (!is.null(covars)) {
            for (covariate_i in covars) {
                if (sum(is.na(sce_pseudo[[covariate_i]])) == ncol(sce_pseudo)) {
                    stop(
                        "Covariate '",
                        covariate_i,
                        "' has all NAs after pseudo-bulking. Might be due to not being a sample-level covariate.",
                        call. = FALSE
                    )
                }
            }
        }

        ## Drop lowly-expressed genes
        message(Sys.time(), " drop lowly expressed genes")
        keep_expr <-
            edgeR::filterByExpr(sce_pseudo, group = "registration_variable")
        sce_pseudo <- sce_pseudo[which(keep_expr), ]

        ## Compute the logcounts
        message(Sys.time(), " normalize expression")
        logcounts(sce_pseudo) <-
            edgeR::cpm(edgeR::calcNormFactors(sce_pseudo),
                log = TRUE,
                prior.count = 1
            )

        ## Done!
        return(sce_pseudo)
    }
