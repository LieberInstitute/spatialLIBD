#' Make a [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class] object `spatialLIBD`-compatible
#' 
#' Adds gene info from a reference GTF, SpaceRanger analysis results, and other
#' information to make a [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object compatible with `spatialLIBD::run_app()`.
#' 
#' @inheritParams read10xVisiumWrapper
#' @param spe A [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object, such as one created by initially running `VisiumIO::TENxVisium()` or
#' `VisiumIO::TENxVisiumHD()`.
#' 
#' @return A [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]
#' object with gene info, SpaceRanger analysis results, and other information
#' attached.
spe_add_info <- function(
        spe,
        samples,
        sample_id = NULL,
        reference_gtf = NULL,
        chrM = "chrM",
        gtf_cols = c(
            "source", "type", "gene_id", "gene_version", "gene_name",
            "gene_type"
        ),
        add_analysis = TRUE,
        verbose = TRUE
    ) {
    if (!all(c("gene_name", "gene_id") %in% gtf_cols)) {
        stop("'gtf_cols' must include 'gene_name' and 'gene_id'", call. = FALSE)
    }

    #   Attempt to guess the GTF path using the Spaceranger web_summary.html
    if (missing(reference_gtf)) {
        summary_file <- file.path(samples[1], "web_summary.html")
        web <- readLines(summary_file)

        #   For spaceranger versions before 3.0
        reference_path <- gsub(
            '.*"',
            "",
            regmatches(
                web, regexpr('\\["Reference Path", *"[/|A-z|0-9|-]+', web)
            )
        )

        #   For recent spaceranger versions (3.0.0+?)
        if (length(reference_path) == 0) {
            reference_path <- sub(
                ".*--transcriptome=(\\S*).*",
                "\\1",
                web[grep("--transcriptome=", web)]
            )
        }
        reference_gtf <- list.files(
            file.path(reference_path, "genes"), "^genes\\.gtf(\\.gz)?$",
            full.names = TRUE
        )
    }
    reference_gtf <- reference_gtf[file.exists(reference_gtf)]
    if (length(reference_gtf) > 1) {
        stop(
            "More than one 'reference_gtf' was provided or detected. Manually specify the path to just one 'reference_gtf'. If different GTF files were used, then different genes will have been quantified and thus cannot be merged naively into a single SpatialExperiment object. If that's the case, we recommend you build separate SPE objects based on the different 'reference_gtf' files used.",
            call. = FALSE
        )
    } else if (length(reference_gtf) == 0) {
        stop(
            "No 'reference_gtf' files were detected. Please check that the files are available.",
            call. = FALSE
        )
    }

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
        match_genes <- match(
            gsub("\\..*", "", rownames(spe)), gsub("\\..*", "", gtf$gene_id)
        )
    }

    if (any(is.na(match_genes))) {
        warning(
            "Dropping ", sum(is.na(match_genes)), " out of ",
            length(match_genes),
            " genes for which we don't have information on the reference GTF file. This typically happens when you are not using the same GTF file as the one that was used by SpaceRanger.",
            call. = FALSE
        )
        ## Drop the few genes for which we don't have information
        spe <- spe[!is.na(match_genes), ]
        match_genes <- match_genes[!is.na(match_genes)]
    }

    ## Keep only some columns from the gtf
    mcols(gtf) <- mcols(gtf)[, gtf_cols[gtf_cols %in% colnames(mcols(gtf))]]

    ## Add the gene info to our SPE object
    rowRanges(spe) <- gtf[match_genes]

    #   Add Spaceranger analysis results if applicable
    if (add_analysis) {
        if (verbose) {
            message(
                Sys.time(),
                " read10xVisiumAnalysis: reading analysis output from SpaceRanger"
            )
        }
        visium_analysis <- read10xVisiumAnalysis(
            samples = samples, sample_id = sample_id
        )

        if (verbose) {
            message(
                Sys.time(),
                " add10xVisiumAnalysis: adding analysis output from SpaceRanger"
            )
        }
        spe <- add10xVisiumAnalysis(spe, visium_analysis)
    }
    
    ## Add information used by spatialLIBD
    if (verbose) message(Sys.time(), " adding information used by spatialLIBD")
    spe <- add_key(spe)
    spe$sum_umi <- colSums(counts(spe))
    spe$sum_gene <- colSums(counts(spe) > 0)
    rowData(spe)$gene_search <- paste0(
        rowData(spe)$gene_name, "; ", rowData(spe)$gene_id
    )
    is_mito <- which(seqnames(spe) == chrM)
    spe$expr_chrM <- colSums(counts(spe)[is_mito, , drop = FALSE])
    spe$expr_chrM_ratio <- spe$expr_chrM / spe$sum_umi
    ## Add a variable for saving the manual annotations
    spe$ManualAnnotation <- "NA"

    ## Done!
    return(spe)
}
