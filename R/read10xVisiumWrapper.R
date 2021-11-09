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
    verbose = TRUE) {
    stopifnot(all(c("gene_name", "gene_id") %in% gtf_cols))

    if (missing(reference_gtf)) {
        summary_file <- file.path(samples[1], "web_summary.html")
        web <- readLines(summary_file)
        reference_path <- gsub('.*"', "", regmatches(web, regexpr('\\["Reference Path", "[/|A-z|0-9|-]+', web)))
        reference_gtf <- file.path(reference_path, "genes", "genes.gtf")
    }
    stopifnot(file.exists(reference_gtf))

    if (verbose) message(Sys.time(), " read10xVisium: reading basic data from SpaceRanger")
    spe <- SpatialExperiment::read10xVisium(
        samples = samples,
        sample_id = sample_id,
        type = type,
        data = data,
        images = images,
        load = load
    )

    if (verbose) message(Sys.time(), " SpatialExperiment::read10xVisiumAnalysis: reading analysis output from SpaceRanger")
    visium_analysis <- read10xVisiumAnalysis(
        samples = samples,
        sample_id = sample_id
    )

    if (verbose) message(Sys.time(), " add10xVisiumAnalysis: adding analysis output from SpaceRanger")
    spe <- add10xVisiumAnalysis(spe, visium_analysis)

    ## Read in the gene information from the annotation GTF file
    if (verbose) message(Sys.time(), " rtracklayer::import: reading the reference GTF file")
    gtf <- rtracklayer::import(reference_gtf)
    gtf <- gtf[gtf$type == "gene"]
    names(gtf) <- gtf$gene_id

    ## Match the genes
    if (verbose) message(Sys.time(), " adding gene information to the SPE object")
    match_genes <- match(rownames(spe), gtf$gene_id)

    if (all(is.na(match_genes))) {
        ## Protect against scenario where one set has GENCODE IDs and the other one has ENSEMBL IDs.
        warning("Gene IDs did not match. This typically happens when you are not using the same GTF file as the one that was used by SpaceRanger. For example, one file uses GENCODE IDs and the other one ENSEMBL IDs. read10xVisiumWrapper() will try to convert them to ENSEMBL IDs.", call. = FALSE)
        match_genes <- match(gsub("\\..*", "", rownames(spe)), gsub("\\..*", "", gtf$gene_id))
    }

    if (any(is.na(match_genes))) {
        warning("Dropping ", sum(is.na(match_genes)), " out of ", length(match_genes), " genes for which we don't have information on the reference GTF file. This typically happens when you are not using the same GTF file as the one that was used by SpaceRanger.", call. = FALSE)
        ## Drop the few genes for which we don't have information
        spe <- spe[!is.na(match_genes), ]
        match_genes <- match_genes[!is.na(match_genes)]
    }

    ## Keep only some columns from the gtf
    mcols(gtf) <- mcols(gtf)[, gtf_cols[gtf_cols %in% colnames(mcols(gtf))]]

    ## Add the gene info to our SPE object
    rowRanges(spe) <- gtf[match_genes]

    ## Add information used by spatialLIBD
    if (verbose) message(Sys.time(), " adding information used by spatialLIBD")
    spe$key <- paste0(colnames(spe), "_", spe$sample_id)
    spe$sum_umi <- colSums(counts(spe))
    spe$sum_gene <- colSums(counts(spe) > 0)
    rowData(spe)$gene_search <- paste0(rowData(spe)$gene_name, "; ", rowData(spe)$gene_id)
    is_mito <- which(seqnames(spe) == chrM)
    spe$expr_chrM <- colSums(counts(spe)[is_mito, , drop = FALSE])
    spe$expr_chrM_ratio <- spe$expr_chrM / spe$sum_umi
    ## Add a variable for saving the manual annotations
    spe$ManualAnnotation <- "NA"

    ## Done!
    return(spe)
}
