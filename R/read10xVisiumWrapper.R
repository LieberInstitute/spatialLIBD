#' Load data from a 10x Genomics Visium experiment and make it spatialLIBD-ready
#'
#' This function expands [SpatialExperiment::read10xVisium()] to include
#' analysis results from SpaceRanger by 10x Genomics as well as add information
#' needed by `run_app()` to visualize the data with the `spatialLIBD` shiny
#' web application.
#'
#' @param samples Passed to [SpatialExperiment::read10xVisium()].
#' @param sample_id Passed to [SpatialExperiment::read10xVisium()].
#' @param type Passed to [SpatialExperiment::read10xVisium()].
#' @param data Passed to [SpatialExperiment::read10xVisium()].
#' @param images Passed to [SpatialExperiment::read10xVisium()].
#' @param load Passed to [SpatialExperiment::read10xVisium()].
#' @param reference_gtf A `character(1)` specifying the path to the reference
#' `genes.gtf` file. If not specified, it will be automatically inferred from
#' the `web_summary.html` file for the first `samples`.
#' @param chrM A `character(1)` specifying the chromosome name of the
#' mitochondrial chromosome. Defaults to `chrM`.
#' @param gtf_cols A `character()` specifying which columns to keep from the GTF
#' file. `"gene_name"` and `"gene_id"` have to be included in `gtf_cols`.
#' @param verbose A `logical(1)` specifying whether to show progress updates.
#' @param add_analysis A `logical(1)` specifying whether to read and add
#' SpaceRanger analysis results.
#'
#' @return A [SpatialExperiment][SpatialExperiment-class] object with the
#' clustering and dimension reduction (projection) results from SpaceRanger by
#' 10x Genomics as well as other information used by `run_app()` for visualzing
#' the gene expression data.
#' @export
#' @importFrom SpatialExperiment read10xVisium
#' @importFrom rtracklayer import
#' @importMethodsFrom Matrix colSums
#' @importFrom SummarizedExperiment "rowRanges<-" "rowData<-"
#' @importFrom S4Vectors "mcols<-" mcols
#' @importFrom BiocGenerics which
#' @importFrom GenomicRanges seqnames
#' @family Utility functions for reading data from SpaceRanger output by 10x
#' Genomics
#'
#' @examples
#' ## See 'Using spatialLIBD with 10x Genomics public datasets' for
#' ## a full example using this function.
#' if (interactive()) {
#'     browseVignettes(package = "spatialLIBD")
#' }
#'
#' ## Note that ?SpatialExperiment::read10xVisium doesn't include all the files
#' ## we need to illustrate read10xVisiumWrapper().
read10xVisiumWrapper <- function(samples = "",
    sample_id = paste0("sample", sprintf("%02d", seq_along(samples))),
    type = c("HDF5", "sparse"),
    data = c("filtered", "raw"),
    images = c("lowres", "hires", "detected", "aligned"),
    load = TRUE,
    reference_gtf = NULL,
    chrM = "chrM",
    gtf_cols = c("source", "type", "gene_id", "gene_version", "gene_name", "gene_type"),
    verbose = TRUE,
    add_analysis = TRUE
    ) {
    if (!all(c("gene_name", "gene_id") %in% gtf_cols)) {
        stop("'gtf_cols' must include 'gene_name' and 'gene_id'", call. = FALSE)
    }

    if (verbose) message(Sys.time(), " SpatialExperiment::read10xVisium: reading basic data from SpaceRanger")
    spe <- SpatialExperiment::read10xVisium(
        samples = samples,
        sample_id = sample_id,
        type = type,
        data = data,
        images = images,
        load = load
    )

    spe <- spe_add_info(
        spe = spe,
        samples = samples,
        sample_id = sample_id,
        reference_gtf = reference_gtf,
        chrM = chrM,
        gtf_cols = gtf_cols,
        add_analysis = add_analysis,
        verbose = verbose
    )

    ## Done!
    return(spe)
}
