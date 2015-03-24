## environmental stuff

# packages
library(MODIS)
library(Rsenal)
library(doParallel)

cl <- makeCluster(3)
registerDoParallel(cl)

# functions
source("R/qcMyd17.R")
source("R/qcxMyd15.R")

# path: modis, raw and extracted
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

# modis options
MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)

# path: modis, processed
ch_dir_mod15 <- "/media/permanent/phd/scintillometer/data/mod15a2/"
ch_dir_myd15 <- "/media/permanent/phd/scintillometer/data/myd15a2/"

# path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data: lai-2200

ch_fls_lai <- list.files("../../phd/lai-2200", pattern = ".TXT$", 
                         full.names = TRUE)

# research sites
ch_plt <- basename(ch_fls_lai)
ls_plt <- strsplit(ch_plt, "\\.")
ch_plt <- sapply(ls_plt, "[[", 1)

# lai-2200 datasets
ls_lai <- lapply(ch_fls_lai, function(i) {
  tmp_ch_rl <- readLines(i)
  
  # date
  tmp_int_id_dt <- grep("DATE", tmp_ch_rl)
  tmp_ch_dt <- tmp_ch_rl[tmp_int_id_dt]
  tmp_ls_dt <- strsplit(tmp_ch_dt, "\t")
  tmp_ch_dt <- sapply(tmp_ls_dt, "[[", 2)
  tmp_ch_dt <- substr(tmp_ch_dt, 1, 8)
  tmp_dt <- as.Date(tmp_ch_dt, format = "%Y%m%d")
  
  # lai
  tmp_int_id_lai <- grep("LAI\t", tmp_ch_rl)
  tmp_ch_lai <- tmp_ch_rl[tmp_int_id_lai]
  tmp_ls_lai <- strsplit(tmp_ch_lai, "\t")
  tmp_ch_lai <- sapply(tmp_ls_lai, "[[", 2)
  tmp_num_lai <- as.numeric(tmp_ch_lai)
  
  tmp_df <- data.frame(Date = tmp_dt, LAI = tmp_num_lai)
  return(tmp_df)
})
df_lai <- do.call("rbind", ls_lai)

# merge
df_plt_lai <- data.frame(PlotID = ch_plt, df_lai)


## data: modis

for (product in c("MOD15A2", "MYD15A2"))
  runGdal(product, begin = "2013001", tileH = 21, tileV = 9, 
          SDSstring = "011100", job = "LAI1km", outProj = "+init=epsg:21037")

# modis dates
ch_fls_lai <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                         pattern = paste0("^MYD15A2.*Lai_1km.tif$"))
ls_fls_lai <- extractDate(ch_fls_lai, asDate = TRUE)
dt_fls_lai <- ls_fls_lai$inputLayerDates

# extent
rst_tmp <- kiliAerial(minNumTiles = 9L, rasterize = TRUE)

# data import and cropping
ls_lai_crp <- foreach(sensor = c("MOD15A2", "MYD15A2"), 
                      path_out = c(ch_dir_mod15, ch_dir_myd15)) %do% {
  ls_myd17_crp <- foreach(sds = c("\\.Lai_", "FparLai", "FparExtra"), 
                          .packages = c("raster", "rgdal", "foreach")) %dopar% {
    tmp_ch_fls <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                             pattern = paste(sensor, sds, ".tif$", sep = ".*"))
    
    tmp_rst_crp <- foreach(i = tmp_ch_fls, .combine = "stack") %do% {
      tmp_rst <- raster(i)
      
      # application of scale factor
      if (sds == "\\.Lai_")
        tmp_rst <- tmp_rst * 0.1
      
      # crop by kili extent
      crop(tmp_rst, rst_tmp, format = "GTiff", overwrite = TRUE,
           filename = paste0(path_out, "/crp/CRP_", basename(i)))
    }
  
    return(tmp_rst_crp)
  }
}

rst_lai <- sapply(ls_lai_crp, "[[", 1)
rst_qc <- sapply(ls_lai_crp, "[[", 2)
rst_xqc <- sapply(ls_lai_crp, "[[", 3)

# quality check
# ls_lai_crp_qc <- foreach(lai = ls_lai_crp, qc = rst_qc, 
#                          path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
#   foreach(i = 1:nlayers(lai), j = names(lai),
#           .packages = c("rgdal", "raster"), .combine = "stack") %dopar% {
#     overlay(lai[[i]], qc[[i]], fun = function(x, y) {
#       num_x <- x[]
#       num_y <- y[]
#       num_y_qc <- sapply(num_y, qcMyd17)
#       log_y_qc <- is.na(num_y_qc)
#       num_x[log_y_qc] <- NA
#       return(num_x)
#     }, filename = paste0(path_out, "/qc/QC_", j), format = "GTiff", 
#     overwrite = TRUE)
#   }
# }

# merge qc data
# rst_lai_crp_qc_mrg <- 
#   foreach(lai_terra = unstack(ls_lai_crp_qc[[1]]), 
#           lai_aqua = unstack(ls_lai_crp_qc[[2]]), 
#           .packages = c("raster", "rgdal"), .combine = "stack") %dopar% {
#     overlay(lai_terra, lai_aqua, fun = function(x, y) {
#       log_isna_x <- is.na(x)
#       log_isna_y <- is.na(y)
#       log_isna <- cbind(log_isna_x, log_isna_y)
#       
#       num_lai_mrg <- sapply(1:nrow(log_isna), function(i) {
#         if (all(log_isna[i, ])) {
#           return(NA)
#         } else if (all(!log_isna[i, ])) {
#           tmp_mu <- mean(x[i], y[i])
#           return(tmp_mu)
#         } else if (!log_isna_x[i] & log_isna_y[i]) {
#           return(x[i])
#         } else {
#           return(y[i])
#         }
#       })
#       return(num_lai_mrg)
#     }, filename = paste0(ch_dir_myd15, "/qc_mrg/MRG_", names(lai_aqua)), 
#     format = "GTiff", overwrite = TRUE)
# }

fls_lai_crp_qc_mrg <- list.files(ch_dir_myd15, pattern = "^MRG_QC_", 
                                 full.names = TRUE, recursive = TRUE)
rst_lai_crp_qc_mrg <- stack(fls_lai_crp_qc_mrg)

# extra quality check
# ls_lai_crp_qc_qcx <- foreach(lai = ls_lai_crp_qc, xqc = rst_xqc, 
#                              path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
#   foreach(i = 1:nlayers(lai), j = names(lai), 
#           .packages = c("rgdal", "raster"), .combine = "stack") %dopar% {
#     overlay(lai[[i]], xqc[[i]], fun = function(x, y) {
#       num_x <- x[]
#       num_y <- y[]
#       num_y_qc <- sapply(num_y, qcxMyd15)
#       log_y_qc <- is.na(num_y_qc)
#       num_x[log_y_qc] <- NA
#       return(num_x)
#     }, filename = paste0(path_out, "/qcx/QCX_", j), format = "GTiff", 
#     overwrite = TRUE)
#   }
# }

# merge qcx data
# rst_lai_crp_qc_qcx_mrg <- 
#   foreach(lai_terra = unstack(ls_lai_crp_qc_qcx[[1]]), 
#           lai_aqua = unstack(ls_lai_crp_qc_qcx[[2]]), 
#           .packages = c("raster", "rgdal"), .combine = "stack") %dopar% {
#             overlay(lai_terra, lai_aqua, fun = function(x, y) {
#               log_isna_x <- is.na(x)
#               log_isna_y <- is.na(y)
#               log_isna <- cbind(log_isna_x, log_isna_y)
#               
#               num_lai_mrg <- sapply(1:nrow(log_isna), function(i) {
#                 if (all(log_isna[i, ])) {
#                   return(NA)
#                 } else if (all(!log_isna[i, ])) {
#                   tmp_mu <- mean(x[i], y[i])
#                   return(tmp_mu)
#                 } else if (!log_isna_x[i] & log_isna_y[i]) {
#                   return(x[i])
#                 } else {
#                   return(y[i])
#                 }
#               })
#               return(num_lai_mrg)
#             }, filename = paste0(ch_dir_myd15, "/qcx_mrg/MRG_", names(lai_aqua)), 
#             format = "GTiff", overwrite = TRUE)
#           }

fls_lai_crp_qc_qcx_mrg <- list.files(ch_dir_myd15, pattern = "^MRG_QCX_", 
                                     full.names = TRUE, recursive = TRUE)
rst_lai_crp_qc_qcx_mrg <- stack(fls_lai_crp_qc_qcx_mrg)
