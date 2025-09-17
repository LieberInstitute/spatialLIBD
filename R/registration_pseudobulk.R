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
#' statistics. This should be a categorical variable, with all categories
#' syntaticly valid (could be used as an R variable, no special characters or
#' leading numbers), ex. 'L1.2', 'celltype2' not 'L1/2' or '2'.
#' @param var_sample_id A `character(1)` specifying the `colData(sce)` variable
#' with the sample ID.
#' @param covars A `character()` with names of sample-level covariates.
#' @param pseudobulk_rds_file A `character(1)` specifying the path for saving
#' an RDS file with the pseudo-bulked object. It's useful to specify this since
#' pseudo-bulking can take hours to run on large datasets.
#' @param min_ncells An `integer(1)` greater than 0 specifying the minimum
#' number of cells (for scRNA-seq) or spots (for spatial) that are combined
#' when pseudo-bulking. Pseudo-bulked samples with less than `min_ncells` on
#' `sce_pseudo$ncells` will be dropped.
#' @param filter_expr A `logical(1)` specifying whether to filter pseudobulked
#' counts with `edgeR::filterByExpr`. Defaults to `TRUE`, filtering is recommended for
#' spatail registratrion workflow.
#' @param mito_gene An optional `logical()` vector indicating which genes are
#' mitochondrial, used to calculate pseudo bulked mitochondrial expression rate
#' `expr_chrM` and `pseudo_expr_chrM`. The length has to match the `nrow(sce)`.
#'
#' @return A pseudo-bulked [SingleCellExperiment-class][SingleCellExperiment::SingleCellExperiment-class] object. The `logcounts()` assay are `log2-CPM`
#' values calculated with `edgeR::cpm(log = TRUE)`. See
#' <https://github.com/LieberInstitute/spatialLIBD/issues/106> and
#' <https://support.bioconductor.org/p/9161754> for more details about the
#' math behind `scuttle::logNormFactors()`, `edgeR::cpm()`, and their
#' differences.
#' @importFrom SingleCellExperiment logcounts
#' @importFrom scuttle aggregateAcrossCells
#' @importFrom edgeR filterByExpr calcNormFactors
#' @importFrom SpatialExperiment "spatialCoords<-"
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
#' rowData(sce)$gene_id <- paste0("ENSG", seq_len(nrow(sce)))
#' rowData(sce)$gene_name <- paste0("gene", seq_len(nrow(sce)))
#'
#' ## Pseudo-bulk by Cell Cycle
#' sce_pseudo <- registration_pseudobulk(
#'     sce,
#'     var_registration = "Cell_Cycle",
#'     var_sample_id = "sample_id",
#'     covars = c("age"),
#'     min_ncells = NULL
#' )
#' colData(sce_pseudo)
#' rowData(sce_pseudo)
registration_pseudobulk <-
    function(
        sce,
        var_registration,
        var_sample_id,
        covars = NULL,
        min_ncells = 10,
        pseudobulk_rds_file = NULL,
        filter_expr = TRUE,
        mito_gene = NULL
    ) {
        ## Check that inputs are correct
        stopifnot(is(sce, "SingleCellExperiment"))
        stopifnot(var_registration %in% colnames(colData(sce)))
        stopifnot(var_sample_id %in% colnames(colData(sce)))
        stopifnot(all(
            !c("registration_sample_id", "registration_variable") %in%
                colnames(colData(sce))
        ))

        ## Avoid any incorrect inputs that are otherwise hard to detect
        stopifnot(!var_registration %in% covars)
        stopifnot(!var_sample_id %in% covars)
        stopifnot(var_registration != var_sample_id)

        ## create var_registration col
        sce$var_registration <- sce[[var_registration]]

        ## Check that the values in the registration variable are numeric
        if (is.numeric(sce[["var_registration"]])) {
            warning(
                sprintf(
                    "var_registration \"%s\" is numeric, convering to categorical vector...",
                    var_registration
                ),
                call. = FALSE
            )
        }

        ## check for Non-Syntactic variables - convert with make.names & warn
        uniq_var_regis <- unique(sce[["var_registration"]])
        syntatic <- grepl(
            "^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$",
            uniq_var_regis
        )
        if (!all(syntatic)) {
            warning(
                sprintf(
                    "var_registration \"%s\" contains non-syntatic variables: %s\nconverting to %s",
                    var_registration,
                    paste(uniq_var_regis[!syntatic], collapse = ", "),
                    paste(
                        make.names(uniq_var_regis[!syntatic]),
                        collapse = ", "
                    )
                ),
                call. = FALSE
            )
            sce[["var_registration"]] <- make.names(sce[["var_registration"]])
        }

        ## Pseudo-bulk for our current BayesSpace cluster results
        message(Sys.time(), " make pseudobulk object")
        ## I think this needs counts assay
        sce_pseudo <- scuttle::aggregateAcrossCells(
            sce,
            DataFrame(
                registration_variable = sce[["var_registration"]],
                registration_sample_id = sce[[var_sample_id]]
            )
        )
        colnames(sce_pseudo) <-
            paste0(
                sce_pseudo$registration_sample_id,
                "_",
                sce_pseudo$registration_variable
            )

        ## rm sce_pseudo$var_registration - redundant with registration variable
        sce_pseudo$var_registration <- NULL

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

        ## Drop pseudo-bulked samples that had low initial contribution
        ## of raw-samples. That is, pseudo-bulked samples that are not
        ## benefiting from the pseudo-bulking process to obtain higher counts.
        if (!is.null(min_ncells)) {
            message(
                Sys.time(),
                " dropping ",
                sum(sce_pseudo$ncells < min_ncells),
                " pseudo-bulked samples that are below 'min_ncells'."
            )
            sce_pseudo <- sce_pseudo[, sce_pseudo$ncells >= min_ncells]
        }

        if (is.factor(sce_pseudo$registration_variable)) {
            ## Drop unused var_registration levels if we had to drop some due
            ## to min_ncells
            sce_pseudo$registration_variable <- droplevels(
                sce_pseudo$registration_variable
            )
        }

        ## compute pseudo QC metrics
        sce_pseudo$pseudo_sum_umi <- colSums(counts(sce_pseudo))

        ## if mitochondrial genes are indicated, calculate pseudo mito rate
        if (!is.null(mito_gene)) {
            if (length(mito_gene) == nrow(sce_pseudo)) {
                sce_pseudo$pseudo_expr_chrM <- colSums(counts(sce_pseudo)[
                    mito_gene,
                    ,
                    drop = FALSE
                ])
                sce_pseudo$pseudo_expr_chrM_ratio <- sce_pseudo$pseudo_expr_chrM /
                    sce_pseudo$pseudo_sum_umi
            } else {
                warning(
                    "length(mito_gene) != nrow(sce_pseudo) : unable to calc 'pseudo_expr_chrM' metrics"
                )
            }
        }

        ## Drop lowly-expressed genes
        if (filter_expr) {
            message(Sys.time(), " drop lowly expressed genes")
            keep_expr <-
                edgeR::filterByExpr(
                    sce_pseudo,
                    group = sce_pseudo$registration_variable
                )
            sce_pseudo <- sce_pseudo[which(keep_expr), ]
        }

        ## Compute the logcounts
        message(Sys.time(), " normalize expression")
        logcounts(sce_pseudo) <-
            edgeR::cpm(
                edgeR::calcNormFactors(sce_pseudo),
                log = TRUE,
                prior.count = 1
            )

        if (is(sce_pseudo, "SpatialExperiment")) {
            ## Drop things we don't need
            spatialCoords(sce_pseudo) <- NULL
            imgData(sce_pseudo) <- NULL
        }

        ## if gene_anme and gene_id cols are available add gene_search to rowData
        if (all(c("gene_name", "gene_id") %in% colnames(rowData(sce_pseudo)))) {
            rowData(sce_pseudo)$gene_search <- paste0(
                rowData(sce_pseudo)$gene_name,
                "; ",
                rowData(sce_pseudo)$gene_id
            )
        }

        if (!is.null(pseudobulk_rds_file)) {
            message(Sys.time(), " saving sce_pseudo to ", pseudobulk_rds_file)
            saveRDS(sce_pseudo, file = pseudobulk_rds_file)
        }

        ## Done!
        return(sce_pseudo)
    }
