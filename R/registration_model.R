#' Title
#'
#' @param sce_pseudo
#' @param covars
#' @param var_registration
#'
#' @return
#' @export
#'
#' @examples
#' mod <- registration_model(sce_pseudo, "age")
#' registration_model(sce_pseudo)
#' registration_model(sce_pseudo, "absent")

registration_model <- function(sce_pseudo, covars = NULL, var_registration = "registration_variable") {

    if (is.null(covars)) {
        mat_formula <- eval(str2expression(paste("~", "0", "+", var_registration)))

    } else {
        mat_formula <- eval(str2expression(paste("~", "0", "+", var_registration, "+", paste(covars, collapse = " + "))))

    }

    ### access different elements of formula and check to see if they're in colData(sce_pseudo)
    terms <- attributes(terms(mat_formula))$term.labels
    terms <- terms[!grepl(":", terms)]
    for (i in seq_along(terms)) {
        if (!terms[i] %in% colnames(colData(sce_pseudo))) {
            stop("Formula term '", terms[i], "' is not contained in colData()", call. = FALSE)
        }
    }

    # create matrix where the rownames are the sample:clusters and the columns are the other variables (spatial.cluster + region + age + sex)
    message("Create model matrix - ", Sys.time())
    mod <- model.matrix(mat_formula,
        data = colData(sce_pseudo)
    ) # binarizes factors

    return(mod)

}
