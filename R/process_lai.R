## environmental stuff

# packages
lib <- c("rgdal", "MODIS", "Rsenal", "doParallel", "zoo", "latticeExtra")
jnk <- sapply(lib, function(i) library(i, character.only = TRUE))

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# functions
source("R/slsPlots.R")
source("R/qcMyd17.R")
source("R/qcxMyd15.R")
source("R/slsTemporalRange.R")
source("R/slsGppIndex.R")

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


## data: coordinates

spt_plt <- readOGR(ch_dir_crd, ch_fls_crd)
spt_plt <- subset(spt_plt, PoleType == "AMP")
spt_plt <- subset(spt_plt, PlotID %in% slsPlots())


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
df_plt_lai <- data.frame(plot = ch_plt, df_lai)

df_plt_lai$season <- "r"
int_id_dryssn <- grep("-D", df_plt_lai$plot)
df_plt_lai$season[int_id_dryssn] <- "d"
df_plt_lai$plot <- tolower(df_plt_lai$plot)
df_plt_lai$plot <- substr(df_plt_lai$plot, 1, 4)


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
mat_lai_crp_qc_mrg <- as.matrix(rst_lai_crp_qc_mrg)

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
mat_lai_crp_qc_qcx_mrg <- as.matrix(rst_lai_crp_qc_qcx_mrg)

# gap-filling
rst_lai_crp_qc_mrg_gf <- calc(rst_lai_crp_qc_mrg, fun = function(i) {
  tmp_num_lai <- i
  tmp_log_isna <- is.na(tmp_num_lai)
  
  tmp_num_mav <- rollapply(tmp_num_lai, width = 3, fill = NA, 
                           FUN = function(...) mean(..., na.rm = TRUE))
  
  tmp_num_lai[tmp_log_isna] <- tmp_num_mav[tmp_log_isna]
  return(tmp_num_lai)
}, filename = paste0(ch_dir_myd15, "/gf/GF"), bylayer = TRUE, 
suffix = names(rst_lai_crp_qc_mrg), format = "GTiff", overwrite = TRUE)


## lai extraction 

# 3-by-3 focal matrix per sls plot, or single pixel if `use_mat = FALSE`
ls_sls_lai <- lapply(1:nrow(df_sls_fls), function(i, use_mat = FALSE) {
  tmp_spt_plt <- subset(spt_plt, PlotID == df_sls_fls$plot[i])
  
  tmp_int_plt_px <- cellFromXY(rst_lai_crp_qc_mrg_gf, tmp_spt_plt)
  
  if (use_mat) {
    tmp_int_plt_px_adj <- adjacent(rst_lai_crp_qc_mrg_gf, tmp_int_plt_px, 
                                   sorted = TRUE, directions = 8, include = TRUE, 
                                   pairs = FALSE)
    tmp_mat_lai <- mat_lai_crp_qc_mrg[tmp_int_plt_px_adj, ]
    tmp_nms <- colnames(tmp_mat_lai)
  } else {
    tmp_num_lai <- mat_lai_crp_qc_mrg[tmp_int_plt_px, ]
    tmp_nms <- names(tmp_num_lai)
    tmp_mat_lai <- matrix(tmp_num_lai, 1, byrow = TRUE)
  }
  tmp_df_lai <- data.frame(tmp_mat_lai)
  names(tmp_df_lai) <- tmp_nms
  
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df_lai)
})
df_sls_lai <- do.call("rbind", ls_sls_lai)

# temporal range per plot
df_sls_tmp_rng <- slsTemporalRange(df_sls_fls)

ls_sls_lai_md <- lapply(1:nrow(df_sls_tmp_rng), function(i) {
  # subset lai data by current plot and season
  tmp_df_lai <- subset(df_sls_lai, plot == df_sls_tmp_rng$plot[i] &
                         season == df_sls_tmp_rng$season[i])
  
  # identify lai scene(s) corresponding to sls measurement
  tmp_df_sls_rng <- subset(df_sls_tmp_rng, plot == df_sls_tmp_rng$plot[i] &
                             season == df_sls_tmp_rng$season[i])
  tmp_int_id_lai <- slsGppIndex(df_sls = tmp_df_sls_rng, 
                                dt_fls_gpp = dt_fls_lai, offset = 2)
  
  tmp_df_sls_lai <- data.frame(tmp_df_sls_rng, tmp_df_lai[, tmp_int_id_lai])
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(tmp_df_sls_lai) > 5) {
    tmp_df <- tmp_df_sls_lai[5:ncol(tmp_df_sls_lai)]
    tmp_num_mu <- rowMeans(tmp_df, na.rm = TRUE)
    tmp_df_sls_lai <- data.frame(tmp_df_sls_lai[, 1:4], tmp_num_mu)
  }
  
  # median lai
  data.frame(tmp_df_sls_lai[1, 1:4], 
             lai = median(tmp_df_sls_lai[, 5], na.rm = TRUE))
})
df_sls_lai_md <- do.call("rbind", ls_sls_lai_md)
save("df_sls_lai_md", file = "data/lai.RData")


## visualization: li-cor lai-2200 vs. modis lai

df_licor_modis_lai <- merge(df_sls_lai_md, df_plt_lai, by = c("plot", "season"))

xyplot(LAI ~ lai, data = df_licor_modis_lai, 
       xlab = expression(LAI[MODIS]), ylab = expression(LAI[LICOR]), 
       panel = function(x, y, ...) {
  panel.xyplot(x, y, col = "grey50")
  panel.text(x, y, labels = df_licor_modis_lai$plot, pos = 2, offset = .75)
})

# deregister parallel backend
closeAllConnections()
