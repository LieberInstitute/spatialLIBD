#' Spatial registration: model
#'
#' This function defines the statistical model that will be used for computing
#' the block correlation as well as pairwise statistics. It is useful to check
#' it in case your sample-level covariates need to be casted. For example, an
#' `integer()` variable might have to be casted into a `factor()` if you wish
#' to model it as a categorical variable and not a continuous one.
#'
#' @param sce_pseudo The output of `registration_pseudobulk()`.
#' @param var_registration A `character(1)` specifying the `colData(sce_pseudo)`
#' variable of interest against which will be used for computing the relevant
#' statistics.
#' @inheritParams registration_pseudobulk
#'
#' @return The output of `model.matrix()` which you can inspect to verify that
#' your sample-level covariates are being properly modeled.
#' @export
#' @importFrom stats model.matrix
#' @family spatial registration and statistical modeling functions
#'
#' @examples
#' example("registration_pseudobulk", package = "spatialLIBD")
#' registration_mod <- registration_model(sce_pseudo, "age")
#' head(registration_mod)
#'
registration_model <-
    function(
        sce_pseudo,
        covars = NULL,
        var_registration = "registration_variable") {
        ## Specify a formula without an intercept
        if (is.null(covars)) {
            mat_formula <-
                eval(str2expression(paste("~", "0", "+", var_registration)))
        } else {
            mat_formula <-
                eval(str2expression(paste(
                    "~",
                    "0",
                    "+",
                    var_registration,
                    "+",
                    paste(covars, collapse = " + ")
                )))
        }

        ## Access different elements of formula and check to see if they're in
        ## colData(sce_pseudo)
        terms <- attributes(terms(mat_formula))$term.labels
        terms <- terms[!grepl(":", terms)]
        for (i in seq_along(terms)) {
            if (!terms[i] %in% colnames(colData(sce_pseudo))) {
                stop("Formula term '",
                    terms[i],
                    "' is not contained in colData()",
                    call. = FALSE
                )
            }
        }

        ## Build the model matrix
        message(Sys.time(), " create model matrix")
        mod <- model.matrix(mat_formula,
            data = colData(sce_pseudo)
        )


        if (qr(mod)$rank != ncol(mod)) {
            stop(
                "The resulting model is not full rank. You might have some 'var_registration' levels that are empty which you can drop with 'droplevels()'. Check the output of 'table(sce_pseudo[[var_registration]])'.",
                call. = FALSE
            )
        }

        ## Done!
        return(mod)
    }
