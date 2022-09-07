library("spatialLIBD")

## Maybe subsetting the sce object will help
## with the memory issues at
## https://travis-ci.com/LieberInstitute/spatialLIBD/builds/150177257?utm_medium=notification&utm_source=slack
if (!exists("sce")) sce <- fetch_data(type = "sce", eh = ehub)
lobstr::obj_size(sce)
# 2.08
# GB

## Subsetting didn't help because we still needed to download the full
## object and load it. Details:
## https://travis-ci.com/LieberInstitute/spatialLIBD/builds/150199037?utm_medium=notification&utm_source=slack
sce_sub <- sce[, sce$sample_name == "151673"]
lobstr::obj_size(sce_sub) / 1024^2 ## Convert to MB
# 247
#  MB

## Ok, let's save a smaller version of the file that has
## enough data for the rest to work.
sce_sub <- sce
assays(sce_sub)$counts <- assays(sce_sub)$logcounts <- Matrix::sparseMatrix(
    i = rep(which(rownames(sce_sub) == "ENSG00000110484"), ncol(sce_sub)),
    j = seq_len(ncol(sce_sub)),
    x = assays(sce_sub)$logcounts["ENSG00000110484", ],
    dims = c(nrow(sce_sub), ncol(sce_sub)),
    dimnames = dimnames(sce_sub)
)
lobstr::obj_size(sce_sub) / 1024^2 ## Convert to MB
# 96.5 MB
save(sce_sub, file = here::here("data-raw", "sce_sub_for_vignette.Rdata"))
system2("ls", paste("-lh", here::here("data-raw", "sce_sub_for_vignette.Rdata")))
# 36M Feb 26 17:19 spatialLIBD/data-raw/sce_sub_for_vignette.Rdata
