#' Download the Human DLPFC Visium data from LIBD
#'
#' This function downloads from `ExperimentHub` Visium, Visium Spatial
#' Proteogenomics (Visium-SPG), or single nucleus RNA-seq (snRNA-seq) data
#' and results analyzed by LIBD from multiple projects.
#' If `ExperimentHub` is not available, this function will
#' download the files from Dropbox using [BiocFileCache::bfcrpath()] unless the
#' files are present already at `destdir`. Note that `ExperimentHub` and
#' `BiocFileCache` will cache the data and automatically detect if you have
#' previously downloaded it, thus making it the preferred way to interact with
#' the data.
#'
#' @param type A `character(1)` specifying which file you want to download. It
#' can either be: `sce` for the
#' \linkS4class{SingleCellExperiment}
#' object containing the spot-level data that includes the information for
#' visualizing the clusters/genes on top of the Visium histology, `sce_layer`
#' for the
#' \linkS4class{SingleCellExperiment}
#' object containing the layer-level data (pseudo-bulked from the spot-level),
#' or `modeling_results` for the list of tables with the `enrichment`,
#' `pairwise`, and `anova` model results from the layer-level data. It can also
#' be `sce_example` which is a reduced version of `sce` just for example
#' purposes. The initial version of `spatialLIBD` downloaded data only from
#' <https://github.com/LieberInstitute/HumanPilot>. As of BioC version 3.13
#' `spe` downloads a
#' [SpatialExperiment-class][SpatialExperiment::SpatialExperiment-class]  object.
#' As of version 1.11.6, this function also allows downloading data from the
#' <http://research.libd.org/spatialDLPFC/> and
#' <https://github.com/LieberInstitute/Human_DLPFC_Deconvolution> projects. As
#' of version 1.11.12,
#' data from <https://github.com/LieberInstitute/Visium_SPG_AD> can be
#' downloaded. As of version 1.17.3, data from
#' <https://research.libd.org/visiumStitched_brain/> can be downloaded. As of
#' version 1.23.1, data from <https://research.libd.org/LFF_spatial_ERC/> can be
#' downloaded.
#'
#' @param destdir The destination directory to where files will be downloaded
#' to in case the `ExperimentHub` resource is not available. If you already
#' downloaded the files, you can set this to the current path where the files
#' were previously downloaded to avoid re-downloading them.
#' @param eh An `ExperimentHub` object
#' [ExperimentHub-class][ExperimentHub::ExperimentHub-class].
#' @param bfc A `BiocFileCache` object
#' [BiocFileCache-class][BiocFileCache::BiocFileCache-class]. Used when
#' `eh` is not available.
#'
#' @return The requested object: `sce`, `sce_layer`, `ve` or `modeling_results` that
#' you have to assign to an object. If you didn't you can still avoid
#' re-loading the object by using `.Last.value`.
#'
#' @export
#' @import ExperimentHub
#' @importFrom AnnotationHub query
#' @importFrom methods is
#' @details The data was initially prepared by scripts at
#' https://github.com/LieberInstitute/HumanPilot and further refined by
#' https://github.com/LieberInstitute/spatialLIBD/blob/master/inst/scripts/make-data_spatialLIBD.R.
#'
#'
#' @references
#' Please always cite the `spatialLIBD` publication whenever you use
#' `fetch_data()` to download data.
#'
#' Pardo B, Spangler A, Weber LM, Page SC, Hicks SC, Jaffe AE, Martinowich K, Maynard KR, Collado-Torres L. spatialLIBD: an R/Bioconductor package to visualize spatially-resolved transcriptomics data. BMC Genomics. 2022 Jun 10;23(1):434. doi: 10.1186/s12864-022-08601-w. PubMed PMID: 35689177; PubMed Central PMCID: PMC9188087.
#'
#' Additionally, please cite the relevant publication describing the data
#' generation and initial data analysis for the dataset you are using.
#'
#' * For "sce", "sce_layer", "modeling_results", "sce_example", and "spe" which
#' are files from the `HumanPilot` study
#' <https://github.com/LieberInstitute/HumanPilot> please cite:
#' Maynard KR, Collado-Torres L, Weber LM, Uytingco C, Barry BK, Williams SR, Catallini JL 2nd, Tran MN, Besich Z, Tippani M, Chew J, Yin Y, Kleinman JE, Hyde TM, Rao N, Hicks SC, Martinowich K, Jaffe AE. Transcriptome-scale spatial gene expression in the human dorsolateral prefrontal cortex. Nat Neurosci. 2021 Mar;24(3):425-436. doi: 10.1038/s41593-020-00787-0. Epub 2021 Feb 8. PubMed PMID: 33558695; PubMed Central PMCID: PMC8095368.
#'
#' * For `spatialDLPFC` files <http://research.libd.org/spatialDLPFC/> please
#' cite:
#' Huuki-Myers LA, Spangler A, Eagles NJ, Montgomery KD, Kwon SH, Guo B, Grant-Peters M, Divecha HR, Tippani M, Sriworarat C, Nguyen AB, Ravichandran P, Tran MN, Seyedian A, Hyde TM, Kleinman JE, Battle A, Page SC, Ryten M, Hicks SC, Martinowich K, Collado-Torres L, Maynard KR. A data-driven single-cell and spatial transcriptomic map of the human prefrontal cortex. Science. 2024 May 24;384(6698):eadh1938. doi: 10.1126/science.adh1938. Epub 2024 May 24. PubMed PMID: 38781370; PubMed Central PMCID: PMC11398705.
#'
#' * Note that `spatialDLPFC_snRNAseq` from
#' <https://github.com/LieberInstitute/Human_DLPFC_Deconvolution> was also
#' described in the following publication:
#' Huuki-Myers LA, Montgomery KD, Kwon SH, Cinquemani S, Eagles NJ, Gonzalez-Padilla D, Maden SK, Kleinman JE, Hyde TM, Hicks SC, Maynard KR, Collado-Torres L. Benchmark of cellular deconvolution methods using a multi-assay dataset from postmortem human prefrontal cortex. Genome Biol. 2025 Apr 7;26(1):88. doi: 10.1186/s13059-025-03552-3. PubMed PMID: 40197307; PubMed Central PMCID: PMC11978107.
#'
#' * For `Visium_SPG_AD` files <https://research.libd.org/Visium_SPG_AD> please
#' cite:
#' Kwon SH, Parthiban S, Tippani M, Divecha HR, Eagles NJ, Lobana JS, Williams SR, Mak M, Bharadwaj RA, Kleinman JE, Hyde TM, Page SC, Hicks SC, Martinowich K, Maynard KR, Collado-Torres L. Influence of Alzheimer's disease related neuropathology on local microenvironment gene expression in the human inferior temporal cortex. GEN Biotechnol. 2023 Oct;2(5):399-417. doi: 10.1089/genbio.2023.0019. Epub 2023 Oct 16. PubMed PMID: 39329069; PubMed Central PMCID: PMC11426291.
#'
#' * For `visiumStitched_brain` files
#' <https://research.libd.org/visiumStitched_brain/> please cite:
#' Eagles NJ, Bach SV, Tippani M, Ravichandran P, Du Y, Miller RA, Hyde TM, Page SC, Martinowich K, Collado-Torres L. Integrating gene expression and imaging data across Visium capture areas with visiumStitched. BMC Genomics. 2024 Nov 13;25(1):1077. doi: 10.1186/s12864-024-10991-y. PubMed PMID: 39533203; PubMed Central PMCID: PMC11559125.
#'
#' * For `LFF_spatial_ERC` files <https://research.libd.org/LFF_spatial_ERC/>
#' please cite:
#' Huuki-Myers LA, Divecha HR, Bach SV, Valentine MR, Eagles NJ, Mulvey B, Bharadwaj RA, Zhang R, Evans JR, Grant-Peters M, Miller RA, Kleinman JE, Han S, Hyde TM, Page SC, Weinberger DR, Martinowich K, Ryten M, Maynard KR, Collado-Torres L. APOE E4 Alzheimer's Risk Converges on an Oligodendrocyte Subtype in the Human Entorhinal Cortex. bioRxiv. 2025 Nov 20;. doi: 10.1101/2025.11.20.689483. PubMed PMID: 41332786; PubMed Central PMCID: PMC12667772.
#'
#' @examples
#'
#' ## Download the SingleCellExperiment object
#' ## at the layer-level
#' if (!exists("sce_layer")) sce_layer <- fetch_data("sce_layer")
#'
#' ## Explore the data
#' sce_layer
#'
#' ## How to download and load "spatialDLPFC_snRNAseq"
#' ## A similar process is needed for downloading and loading other
#' ## HDF5SummarizedExperiment files:
#' ## * "LFF_spatial_ERC_SRT"
#' ## * LFF_spatial_ERC_snRNAseq"
#'
#' \dontrun{
#' sce_path_zip <- fetch_data("spatialDLPFC_snRNAseq")
#' sce_path <- unzip(sce_path_zip, exdir = tempdir())
#' sce <- HDF5Array::loadHDF5SummarizedExperiment(
#'     file.path(tempdir(), "sce_DLPFC_annotated")
#' )
#' sce
#' #> class: SingleCellExperiment
#' #> dim: 36601 77604
#' #> metadata(3): Samples cell_type_colors cell_type_colors_broad
#' #> assays(2): counts logcounts
#' #> rownames(36601): MIR1302-2HG FAM138A ... AC007325.4 AC007325.2
#' #> rowData names(7): source type ... gene_type binomial_deviance
#' #> colnames(77604): 1_AAACCCAAGTTCTCTT-1 1_AAACCCACAAGGTCTT-1 ... 19_TTTGTTGTCTCATTGT-1 19_TTTGTTGTCTTAAGGC-1
#' #> colData names(32): Sample Barcode ... cellType_layer layer_annotation
#' #> reducedDimNames(4): GLMPCA_approx TSNE UMAP HARMONY
#' #> mainExpName: NULL
#' #> altExpNames(0):
#' lobstr::obj_size(sce)
#' #> 172.28 MB
#' }
fetch_data <-
    function(
        type = c(
            "sce",
            "sce_layer",
            "modeling_results",
            "sce_example",
            "spe",
            "spatialDLPFC_Visium",
            "spatialDLPFC_Visium_example_subset",
            "spatialDLPFC_Visium_pseudobulk",
            "spatialDLPFC_Visium_modeling_results",
            "spatialDLPFC_Visium_SPG",
            "spatialDLPFC_snRNAseq",
            "Visium_SPG_AD_Visium_wholegenome_spe",
            "Visium_SPG_AD_Visium_targeted_spe",
            "Visium_SPG_AD_Visium_wholegenome_pseudobulk_spe",
            "Visium_SPG_AD_Visium_wholegenome_modeling_results",
            "visiumStitched_brain_spe",
            "visiumStitched_brain_spaceranger",
            "visiumStitched_brain_Fiji_out",
            "LFF_spatial_ERC_SRT",
            "LFF_spatial_ERC_SRT_pseudobulk",
            "LFF_spatial_ERC_SRT_modeling_results",
            "LFF_spatial_ERC_snRNAseq",
            "LFF_spatial_ERC_snRNAseq_pseudobulk_broad",
            "LFF_spatial_ERC_snRNAseq_pseudobulk_subcluster",
            "LFF_spatial_ERC_snRNAseq_modeling_results_broad",
            "LFF_spatial_ERC_snRNAseq_modeling_results_subcluster"
        ),
        destdir = tempdir(),
        eh = ExperimentHub::ExperimentHub(),
        bfc = BiocFileCache::BiocFileCache()
    ) {
        ## Some variables
        sce <-
            sce_layer <- modeling_results <- sce_sub <- spe <- NULL

        ## Choose a type among the valid options
        type <- match.arg(type)

        ## Check inputs
        stopifnot(methods::is(eh, "ExperimentHub"))

        ## Deal with the special case of VisiumExperiment first
        if (type == "spe") {
            spe <- sce_to_spe(fetch_data("sce", destdir = destdir, eh = eh))
            return(spe)
        }

        ## Other pre-BioC 3.12 regular files
        if (type == "sce") {
            if (!enough_ram()) {
                warning(
                    paste(
                        "Your system might not have enough memory available.",
                        "Try with a machine that has more memory",
                        "or use the 'sce_example'."
                    )
                )
            }
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <-
                "Human_Pilot_DLPFC_Visium_spatialLIBD_spot_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/f4wcvtdq428y73p/Human_DLPFC_Visium_processedData_sce_scran_spatialLIBD.Rdata?dl=1"
        } else if (type == "sce_layer") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_layer_level_SCE"

            ## While EH is not set-up
            file_name <-
                "Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata"
            url <-
                "https://www.dropbox.com/s/bg8xwysh2vnjwvg/Human_DLPFC_Visium_processedData_sce_scran_sce_layer_spatialLIBD.Rdata?dl=1"
        } else if (type == "modeling_results") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_Pilot_DLPFC_Visium_spatialLIBD_modeling_results"

            ## While EH is not set-up
            file_name <- "Human_DLPFC_Visium_modeling_results.Rdata"
            url <-
                "https://www.dropbox.com/s/se6rrgb9yhm5gfh/Human_DLPFC_Visium_modeling_results.Rdata?dl=1"
        } else if (type == "sce_example") {
            tag <- "Human_Pilot_DLPFC_Visium_spatialLIBD"
            hub_title <- "Human_DLPFC_Visium_sce_example"

            ## While EH is not set-up
            file_name <- "sce_sub_for_vignette.Rdata"
            url <-
                "https://www.dropbox.com/s/5ra9o8ku9iyyf70/sce_sub_for_vignette.Rdata?dl=1"
        } else if (type == "spatialDLPFC_Visium") {
            if (!enough_ram(7e+09)) {
                warning(
                    paste(
                        "Your system might not have enough memory available (7GB).",
                        "Try with a machine that has more memory."
                    )
                )
            }
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_spe"

            ## While EH is not set-up
            file_name <-
                "spe_filtered_final_with_clusters_and_deconvolution_results.rds"
            url <-
                "https://www.dropbox.com/s/y2ifv5v8g68papf/spe_filtered_final_with_clusters_and_deconvolution_results.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_example_subset") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD_example_subset"
            hub_title <- "spatialDLPFC_Visium_spe_example_subset"

            ## While EH is not set-up
            file_name <- "spatialDLPFC_spe_subset_example.rds"
            url <-
                "https://www.dropbox.com/s/3jm3kjab9lzaemo/spatialDLPFC_spe_subset_example.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_pseudobulk") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_pseudobulk_spe"

            ## While EH is not set-up
            file_name <-
                "sce_pseudo_BayesSpace_k09.rds"
            url <-
                "https://www.dropbox.com/s/pbti4strsfk1m55/sce_pseudo_BayesSpace_k09.rds?dl=1"
        } else if (type == "spatialDLPFC_Visium_modeling_results") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <-
                "modeling_results_BayesSpace_k09.Rdata"
            url <-
                "https://www.dropbox.com/s/srkb2ife75px2yz/modeling_results_BayesSpace_k09.Rdata?dl=1"
        } else if (type == "spatialDLPFC_Visium_SPG") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- "spatialDLPFC_Visium_SPG_spe"

            ## While EH is not set-up
            file_name <-
                "spe.rds"
            url <-
                "https://www.dropbox.com/s/nbf13dna9ibqfaa/spe.rds?dl=1"
        } else if (type == "spatialDLPFC_snRNAseq") {
            tag <- "spatialDLPFC_Visium_VisiumSPG_snRNAseq_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <-
                "sce_DLPFC_annotated.zip"
            url <-
                "https://www.dropbox.com/s/5919zt00vm1ht8e/sce_DLPFC_annotated.zip?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_wholegenome_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_SPG_AD_spe_wholegenome.Rdata"
            url <-
                "https://www.dropbox.com/s/ng036m63grykdm6/Visium_SPG_AD_spe_wholegenome.Rdata?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_targeted_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_SPG_AD_spe_targeted.Rdata"
            url <-
                "https://www.dropbox.com/s/kda9160awc2h8jq/Visium_SPG_AD_spe_targeted.Rdata?dl=1"
        } else if (type == "Visium_SPG_AD_Visium_wholegenome_pseudobulk_spe") {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_pseudo_pathology_wholegenome.rds"
            url <-
                "https://www.dropbox.com/s/p8foxj6t6inb8uf/sce_pseudo_pathology_wholegenome.rds?dl=1"
        } else if (
            type == "Visium_SPG_AD_Visium_wholegenome_modeling_results"
        ) {
            tag <- "Visium_SPG_AD_Alzheimer_Disease_ITC_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "Visium_IF_AD_modeling_results.Rdata"
            url <-
                "https://www.dropbox.com/s/5plupu8bj5m0kfh/Visium_IF_AD_modeling_results.Rdata?dl=1"
        } else if (type == "visiumStitched_brain_spe") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_spe.rds"
            url <-
                "https://www.dropbox.com/scl/fi/9re464y6qaojx3r94nq5u/visiumStitched_brain_spe.rds?rlkey=nq6a82u23xuu9hohr86oodwdi&dl=1"
        } else if (type == "visiumStitched_brain_spaceranger") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_spaceranger.zip"
            url <-
                "https://www.dropbox.com/scl/fi/5jdoaukvhq3v7lk19228y/visiumStitched_brain_spaceranger.zip?rlkey=bdgjc6mgy1ierdad6h6v5g29c&dl=1"
        } else if (type == "visiumStitched_brain_Fiji_out") {
            tag <- "visiumStitched_brain_spatialLIBD"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "visiumStitched_brain_fiji_out.zip"
            url <-
                "https://www.dropbox.com/scl/fi/bevo52e96f2kdwllf8dkk/visiumStitched_brain_fiji_out.zip?rlkey=ptwal8f5zxakzejwd0oqw0lhj&dl=1"
        } else if (type == "LFF_spatial_ERC_SRT") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "spe_ERC_annotated.zip"
            url <-
                "https://www.dropbox.com/scl/fi/jrfc2tllppk52yqm1zyb3/spe_ERC_annotated.zip?rlkey=shi4pn3acf916dtscin32zjsn&dl=1"
        } else if (type == "LFF_spatial_ERC_SRT_pseudobulk") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "spe_pseudobulk-SpD.rds"
            url <-
                "https://www.dropbox.com/scl/fi/2cl002smtcl37vaawodil/spe_pseudobulk-SpD.rds?rlkey=6igag1uggpg7h70ruucbl7leb&dl=1"
        } else if (type == "LFF_spatial_ERC_SRT_modeling_results") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "modeling_results-SpD.rds"
            url <-
                "https://www.dropbox.com/scl/fi/4emtnag2x2usb8t4njvb9/modeling_results-SpD.rds?rlkey=d2qr82rz6uargnthkkahrs7e1&dl=1"
        } else if (type == "LFF_spatial_ERC_snRNAseq") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_ERC_subcluster.zip"
            url <-
                "https://www.dropbox.com/scl/fi/plekynuzpb22ii08pcn4j/sce_ERC_subcluster.zip?rlkey=bsgqc5dx4o0memidhs943qdhx&dl=1"
        } else if (type == "LFF_spatial_ERC_snRNAseq_pseudobulk_broad") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_subcluster_pseudobulk-cell_type_broad.rds"
            url <-
                "https://www.dropbox.com/scl/fi/supihrwunx99ldzztzecm/sce_subcluster_pseudobulk-cell_type_broad.rds?rlkey=uy7zmoifjhetfglrav4i2j0ns&dl=1"
        } else if (type == "LFF_spatial_ERC_snRNAseq_pseudobulk_subcluster") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_subcluster_pseudobulk-cell_type_anno.rds"
            url <-
                "https://www.dropbox.com/scl/fi/c1n4i90k5pny7lij3gvxv/sce_subcluster_pseudobulk-cell_type_anno.rds?rlkey=v3kyay0p2i3h24u7c8fx2dn7u&dl=1"
        } else if (type == "LFF_spatial_ERC_snRNAseq_modeling_results_broad") {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_subcluster_pseudobulk-cell_type_broad.rds"
            url <-
                "https://www.dropbox.com/scl/fi/eu9z1qp3gx5verdadwpyu/sce_subcluster_modeling_results-cell_type_broad.rds?rlkey=lwd66c7x5vdqeky431unanp45&dl=1"
        } else if (
            type == "LFF_spatial_ERC_snRNAseq_modeling_results_subcluster"
        ) {
            tag <- "LFF_spatial_ERC"
            hub_title <- type

            ## While EH is not set-up
            file_name <- "sce_subcluster_pseudobulk-cell_type_anno.rds"
            url <-
                "https://www.dropbox.com/scl/fi/y42pv7k02luvznwqii2rm/sce_subcluster_modeling_results-cell_type_anno.rds?rlkey=17c0ybowjpejdxc71tuxlcfna&dl=1"
        }

        file_path <- file.path(destdir, file_name)
        ## Use local data if present
        if (!file.exists(file_path)) {
            q <-
                AnnotationHub::query(eh, pattern = c(tag, hub_title))

            if (length(q) == 1) {
                ## ExperimentHub has the data =)
                res <- q[[1]]
                if (type %in% c("sce", "sce_example")) {
                    res <- .update_sce(res)
                } else if (type == "sce_layer") {
                    res <- .update_sce_layer(res)
                }
                return(res)
            } else {
                ## ExperimentHub backup: download from Dropbox
                file_path <- BiocFileCache::bfcrpath(bfc, url)
            }
        }

        ## Now load the data if possible
        message(Sys.time(), " loading file ", file_path)
        if (grepl(".Rdata", file_path)) {
            load(file_path, verbose = FALSE)
            if (type == "sce") {
                return(.update_sce(sce))
            } else if (type == "sce_layer") {
                return(.update_sce_layer(sce_layer))
            } else if (
                type == "modeling_results" ||
                    type == "spatialDLPFC_Visium_modeling_results" ||
                    type == "Visium_SPG_AD_Visium_wholegenome_modeling_results"
            ) {
                return(modeling_results)
            } else if (type == "sce_example") {
                return(.update_sce(sce_sub))
            } else if (
                type == "Visium_SPG_AD_Visium_wholegenome_spe" ||
                    type == "Visium_SPG_AD_Visium_targeted_spe"
            ) {
                return(spe)
            }
        } else if (grepl(".rds", file_path)) {
            return(readRDS(file_path))
        } else {
            file_path
        }
    }


.update_sce <- function(sce) {
    ## Rename here the default cluster we want to show in the shiny app
    sce$spatialLIBD <- sce$layer_guess_reordered_short

    ## Add ManualAnnotation which was formerly called Layer, then drop Layer
    sce$ManualAnnotation <- sce$Layer
    sce$Layer <- NULL

    return(sce)
}

.update_sce_layer <- function(sce_layer) {
    ## Rename here the default cluster we want to show in the shiny app
    sce_layer$spatialLIBD <- sce_layer$layer_guess_reordered_short

    return(sce_layer)
}
