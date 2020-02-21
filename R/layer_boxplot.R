#' Title
#'
#' @param sce_layer
#' @param sig_genes
#'
#' @return
#' @references Adapted from
#' https://github.com/LieberInstitute/HumanPilot/blob/master/Analysis/Layer_Guesses/layer_specificity.R
#' @export
#'
#' @examples
#'
#' ori_modeling_results <- fetch_data(type = 'modeling_results')
#' ori_sce_layer <- fetch_data(type = 'sce_layer')
#'
#' ## Top 2 genes from the specificity model
#' sig_genes <- sig_genes_extract_all(n = 2,
#'     modeling_results = ori_modeling_results,
#'     sce_layer = ori_sce_layer)
#'
#' layer_boxplot(sig_genes = sig_genes, sce_layer = ori_sce_layer)
#' layer_boxplot(sig_genes = sig_genes,
#'     short_title = FALSE,
#'     sce_layer = ori_sce_layer)
#'
#' layer_boxplot(
#'     i = which(sig_genes$model_type == 'anova')[1],
#'     sig_genes = sig_genes,
#'     sce_layer = ori_sce_layer
#' )
#' layer_boxplot(
#'     i = which(sig_genes$model_type == 'pairwise')[1],
#'     sig_genes = sig_genes,
#'     sce_layer = ori_sce_layer
#' )
#'
#' ## Viridis colors
#' library('viridisLite')
#' layer_boxplot(
#'     sig_genes = sig_genes,
#'     sce_layer = ori_sce_layer,
#'     col_low_box = viridis(4)[2],
#'     col_low_point = viridis(4)[1],
#'     col_high_box = viridis(4)[3],
#'     col_high_point = viridis(4)[4]
#' )
#'
#' ## Paper colors
#' layer_boxplot(
#'     sig_genes = sig_genes,
#'     sce_layer = ori_sce_layer,
#'     col_low_box = 'palegreen3',
#'     col_low_point = 'springgreen2',
#'     col_high_box = 'darkorange2',
#'     col_high_point = 'orange1'
#' )
#'

layer_boxplot <- function(i = 1,
    sig_genes = sig_genes_extract(),
    short_title = TRUE,
    sce_layer = fetch_data(type = 'sce_layer'),
    col_bkg_box = 'grey80',
    col_bkg_point = 'grey40',
    col_low_box = 'violet',
    col_low_point = 'darkviolet',
    col_high_box = 'skyblue',
    col_high_point = 'dodgerblue4',
    seed = 20200206) {
    ## Extract the logcounts
    mat <- assays(sce_layer)$logcounts

    ## Some internal functions
    add_rest <- function(x) {
        if (grepl('full|noWM', x))
            return(x)
        ifelse(grepl('-', x), x, paste0(x, '-rest'))
    }

    assign_color <- function(x, bkg, high, low) {
        if (grepl('-', x)) {
            col <- rep(bkg, 7)
            col[levels(sce_layer$layer_guess_reordered) == gsub('-.*', '', x)] <-
                high
            col[levels(sce_layer$layer_guess_reordered) == gsub('.*-', '', x)] <-
                low
        } else {
            col <- rep(low, 7)
            col[levels(sce_layer$layer_guess_reordered) == x] <-
                high
            if (grepl('noWM', x))
                col[levels(sce_layer$layer_guess_reordered) == 'WM'] <- bkg
        }
        names(col) <- levels(sce_layer$layer_guess_reordered_short)
        return(col)
    }

    add_bkg_col <- function(x) {
        assign_color(x,
            bkg = col_bkg_box,
            high = col_high_box,
            low = col_low_box)
    }

    add_col <- function(x) {
        assign_color(x,
            bkg = col_bkg_point,
            high = col_high_point,
            low = col_low_point)
    }

    title <- if (short_title) {
        paste(sig_genes$gene[i],
            gsub('-', '>', add_rest(
                gsub('ayer', '', sig_genes$test[i])
            )),
            paste0('p=', formatC(
                sig_genes$pval[i],
                format = "e",
                digits = 2
            )))
    } else {
        paste(
            sig_genes$gene[i],
            sig_genes$ensembl[i],
            sig_genes$layer[i],
            '\n',

            'stat',
            formatC(
                sig_genes$stat[i],
                format = "e",
                digits = 2
            ),
            'p',
            formatC(
                sig_genes$pval[i],
                format = "e",
                digits = 2
            ),

            '\n',
            gsub('top', 'r', gsub(
                'Layer',
                'L',
                sapply(sig_genes$results[i], paste0, collapse = ';')
            ))
        )
    }

    set.seed(seed)
    if (short_title) {
        par(mar = c(2, 5, 2, 1) + 0.1)
    } else {
        par(mar = c(2, 5, 6, 1) + 0.1)
    }

    # message(paste(Sys.time(), 'making the plot for', i, 'gene', sig_genes$gene[i]))
    boxplot(
        mat[sig_genes$gene_index[i], ] ~ sce_layer$layer_guess_reordered_short,
        xlab = '',
        ylab = 'logcounts',
        main = title,
        outline = FALSE,
        cex = 2,
        cex.axis = 2,
        cex.lab = 2,
        cex.main = ifelse(short_title, 2, 1.5),
        col = add_bkg_col(sig_genes$test[i])
    )
    points(
        mat[sig_genes$gene_index[i], ] ~ jitter(as.integer(
            sce_layer$layer_guess_reordered_short
        )),
        pch = 21,
        bg = add_col(sig_genes$test[i])[as.character(sce_layer$layer_guess_reordered_short)],
        cex = 2
    )
}
