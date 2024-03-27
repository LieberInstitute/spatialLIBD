#' Layer-level (group-level) boxplots
#'
#' This function uses the output of [sig_genes_extract_all()] as well as the
#' logcounts from the layer-level (group-level) data to visualize the expression
#' of a given gene and display the modeling results for the given gene.
#'
#' @param i A `integer(1)` indicating which row of `sig_genes` do you want to
#' plot.
#' @param sig_genes The output of [sig_genes_extract_all()].
#' @param short_title A `logical(1)` indicating whether to print a short title
#' or not.
#' @inheritParams sig_genes_extract
#' @param col_bkg_box Box background color for layers not used when visualizing
#' the `pairwise` model results.
#' @param col_bkg_point Similar to `col_bkg_box` but for the points.
#' @param col_low_box Box background color for layer(s) with the expected
#' lower expression based on the actual test for row `i` of `sig_genes`.
#' @param col_low_point Similar to `col_low_box` but for the points.
#' @param col_high_box Similar to `col_low_box` but for the expected layer(s)
#' with higher expression.
#' @param col_high_point Similar to `col_high_box` but for the points.
#' @param cex Controls the size of the text, points and axis legends.
#' @param group_var A `character(1)` specifying a `colData(sce_layer)` column
#' name to use for the x-axis.
#' @param assayname A `character(1)` specifying the default assay to use from
#' `assays(sce_layer)`.
#'
#' @return This function creates a boxplot of the layer-level data
#' (group-level) separated by layer and colored based on the model type from row
#' `i` of `sig_genes`.
#'
#' @references Adapted from
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity.R
#' @export
#' @importFrom SummarizedExperiment assays assay
#' @importFrom graphics par boxplot points
#' @family Layer modeling functions
#'
#' @examples
#'
#' ## Obtain the necessary data
#' if (!exists("modeling_results")) {
#'     modeling_results <- fetch_data(type = "modeling_results")
#' }
#' if (!exists("sce_layer")) sce_layer <- fetch_data(type = "sce_layer")
#'
#' ## Top 2 genes from the enrichment model
#' sig_genes <- sig_genes_extract_all(
#'     n = 2,
#'     modeling_results = modeling_results,
#'     sce_layer = sce_layer
#' )
#'
#' ## Example default boxplot
#' set.seed(20200206)
#' layer_boxplot(sig_genes = sig_genes, sce_layer = sce_layer)
#'
#' ## Now show the long title version
#' set.seed(20200206)
#' layer_boxplot(
#'     sig_genes = sig_genes,
#'     short_title = FALSE,
#'     sce_layer = sce_layer
#' )
#'
#' set.seed(20200206)
#' layer_boxplot(
#'     i = which(sig_genes$model_type == "anova")[1],
#'     sig_genes = sig_genes,
#'     sce_layer = sce_layer
#' )
#'
#' set.seed(20200206)
#' layer_boxplot(
#'     i = which(sig_genes$model_type == "pairwise")[1],
#'     sig_genes = sig_genes,
#'     sce_layer = sce_layer
#' )
#'
#' ## Viridis colors displayed in the shiny app
#' library("viridisLite")
#' set.seed(20200206)
#' layer_boxplot(
#'     sig_genes = sig_genes,
#'     sce_layer = sce_layer,
#'     col_low_box = viridis(4)[2],
#'     col_low_point = viridis(4)[1],
#'     col_high_box = viridis(4)[3],
#'     col_high_point = viridis(4)[4]
#' )
#'
#' ## Paper colors displayed in the shiny app
#' set.seed(20200206)
#' layer_boxplot(
#'     sig_genes = sig_genes,
#'     sce_layer = sce_layer,
#'     col_low_box = "palegreen3",
#'     col_low_point = "springgreen2",
#'     col_high_box = "darkorange2",
#'     col_high_point = "orange1"
#' )
#'
#' ## Blue/red colors displayed in the shiny app
#' set.seed(20200206)
#' layer_boxplot(
#'     i = which(sig_genes$model_type == "pairwise")[1],
#'     sig_genes = sig_genes,
#'     sce_layer = sce_layer,
#'     col_bkg_box = "grey90",
#'     col_bkg_point = "grey60",
#'     col_low_box = "skyblue2",
#'     col_low_point = "royalblue3",
#'     col_high_box = "tomato2",
#'     col_high_point = "firebrick4",
#'     cex = 3
#' )
layer_boxplot <- function(
        i = 1,
        sig_genes = sig_genes_extract(),
        short_title = TRUE,
        sce_layer = fetch_data(type = "sce_layer"),
        col_bkg_box = "grey80",
        col_bkg_point = "grey40",
        col_low_box = "violet",
        col_low_point = "darkviolet",
        col_high_box = "skyblue",
        col_high_point = "dodgerblue4",
        cex = 2,
        group_var = "layer_guess_reordered_short",
        assayname = "logcounts") {
    ## Extract the logcounts (default)
    mat <- assay(sce_layer, assayname)

    ## Some internal functions
    add_rest <- function(x) {
        if (grepl("full|noWM", x)) {
            return(x)
        }
        ifelse(grepl("-", x), x, paste0(x, "-rest"))
    }

    groups <- factor(colData(sce_layer)[[group_var]])
    n_groups <- length(unique(groups))

    assign_color <- function(x, bkg, high, low) {
        if (grepl("-", x)) {
            col <- rep(bkg, n_groups)
            col[levels(groups) == gsub("-.*", "", x)] <-
                high
            col[levels(groups) == gsub(".*-", "", x)] <-
                low
        } else {
            col <- rep(low, n_groups)
            col[levels(groups) == x] <-
                high
            if (grepl("noWM", x)) {
                col[levels(groups) == "WM"] <-
                    bkg
            }
        }
        names(col) <- levels(groups)
        return(col)
    }

    add_bkg_col <- function(x) {
        assign_color(x,
            bkg = col_bkg_box,
            high = col_high_box,
            low = col_low_box
        )
    }

    add_col <- function(x) {
        assign_color(x,
            bkg = col_bkg_point,
            high = col_high_point,
            low = col_low_point
        )
    }

    title <- if (short_title) {
        paste(
            sig_genes$gene[i],
            gsub("-", ">", add_rest(
                gsub("ayer", "", sig_genes$test[i])
            )),
            paste0("p=", formatC(
                sig_genes$pval[i],
                format = "e",
                digits = 2
            ))
        )
    } else {
        paste(
            sig_genes$gene[i],
            sig_genes$ensembl[i],
            sig_genes$layer[i],
            "\n",
            "stat",
            formatC(
                sig_genes$stat[i],
                format = "e",
                digits = 2
            ),
            "p",
            formatC(
                sig_genes$pval[i],
                format = "e",
                digits = 2
            ),
            "\n",
            gsub("top", "r", gsub(
                "Layer",
                "L",
                vapply(sig_genes$results[i], paste0, character(1), collapse = ";")
            ))
        )
    }

    mar_extra <- max(nchar(levels(groups))) %/% 3
    if (short_title) {
        par(mar = c(3 + mar_extra * 2, 7, 3, 1) + 0.1)
    } else {
        par(mar = c(3 + mar_extra * 2, 7, 7, 1) + 0.1)
    }

    # message(paste(Sys.time(), 'making the plot for', i, 'gene', sig_genes$gene[i]))
    boxplot(
        mat[sig_genes$gene_index[i], ] ~ groups,
        xlab = "",
        ylab = assayname,
        main = title,
        outline = FALSE,
        cex = cex,
        cex.axis = cex * 4 / 5,
        cex.lab = cex,
        cex.main = ifelse(short_title, cex, cex * 3 / 4),
        col = add_bkg_col(sig_genes$test[i]),
        ylim = range(mat[sig_genes$gene_index[i], ]),
        las = 2
    )
    points(
        mat[sig_genes$gene_index[i], ] ~ jitter(as.integer(
            groups
        )),
        pch = 21,
        bg = add_col(sig_genes$test[i])[as.character(groups)],
        cex = cex
    )
}
