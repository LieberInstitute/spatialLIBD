# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/Human_DLPFC_Visium_processedData_sce_scran.Rdata data/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/rda_scran/clust_k5_list.Rdata data/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/Analysis/rda_scran/clust_10x_layer_maynard_martinowich.Rdata data/

# cat(sapply(unique(sce$sample_name), function(x) { paste0('mkdir data/', x, '\n') }))
# mkdir data/151507
# mkdir data/151508
# mkdir data/151509
# mkdir data/151510
# mkdir data/151669
# mkdir data/151670
# mkdir data/151671
# mkdir data/151672
# mkdir data/151673
# mkdir data/151674
# mkdir data/151675
# mkdir data/151676

# cat(sapply(unique(sce$sample_name), function(x) { paste0('scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/', x, '/tissue_lowres_image.png data/', x, '/\n') }))
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151507/tissue_lowres_image.png data/151507/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151508/tissue_lowres_image.png data/151508/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151509/tissue_lowres_image.png data/151509/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151510/tissue_lowres_image.png data/151510/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151669/tissue_lowres_image.png data/151669/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151670/tissue_lowres_image.png data/151670/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151671/tissue_lowres_image.png data/151671/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151672/tissue_lowres_image.png data/151672/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151673/tissue_lowres_image.png data/151673/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151674/tissue_lowres_image.png data/151674/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151675/tissue_lowres_image.png data/151675/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151676/tissue_lowres_image.png data/151676/

# cat(sapply(unique(sce$sample_name), function(x) { paste0('scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/', x, '/tissue_hires_image.png data/', x, '/\n') }))
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151507/tissue_hires_image.png data/151507/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151508/tissue_hires_image.png data/151508/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151509/tissue_hires_image.png data/151509/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151510/tissue_hires_image.png data/151510/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151669/tissue_hires_image.png data/151669/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151670/tissue_hires_image.png data/151670/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151671/tissue_hires_image.png data/151671/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151672/tissue_hires_image.png data/151672/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151673/tissue_hires_image.png data/151673/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151674/tissue_hires_image.png data/151674/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151675/tissue_hires_image.png data/151675/
# scp e:/dcl02/lieber/ajaffe/SpatialTranscriptomics/HumanPilot/10X/151676/tissue_hires_image.png data/151676/

library('rsconnect')
# setwd(here::here())
load('.deploy_info.Rdata')
rsconnect::setAccountInfo(name = deploy_info$name,
    token = deploy_info$token,
    secret = deploy_info$secret)
options(repos = BiocManager::repositories())
deployApp(
    appFiles = c(
        'ui.R',
        'server.R',
        'global.R',
        dir('data', full.names = TRUE, recursive = TRUE)
    ),
    appName = 'spatialLIBD',
    account = 'jhubiostatistics',
    server = 'shinyapps.io'
)
Y
