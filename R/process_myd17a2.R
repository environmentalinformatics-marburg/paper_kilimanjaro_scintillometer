## environmental stuff

# packages
library(rgdal)
library(MODIS)
library(Rsenal)
library(doParallel)
library(zoo)
library(GSODTools)

# functions
source("R/slsTemporalRange.R")
source("R/slsGppIndex.R")

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# path: modis, raw and extracted
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

# path: modis, processed
ch_dir_myd17 <- "/media/permanent/phd/scintillometer/data/myd17a2/"

# path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

# path: output storage
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"

# modis options
MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)


## data

# data download
for (product in c("MOD17A2", "MYD17A2"))
  runGdal(product, begin = "2013001", tileH = 21, tileV = 9, 
          SDSstring = "101", job = "GPPkili", outProj = "+init=epsg:21037")

# modis dates
ch_fls_gpp <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                         pattern = paste0("^MYD17A2.*Gpp_1km.tif$"))
ls_fls_gpp <- extractDate(ch_fls_gpp, asDate = TRUE)
dt_fls_gpp <- ls_fls_gpp$inputLayerDates

# extent
rst_tmp <- kiliAerial(minNumTiles = 9L, rasterize = TRUE)

# data import and cropping
ls_myd17_crp <- foreach(sds = c("Gpp", "QC"), 
                        .packages = c("raster", "rgdal", "foreach")) %dopar% {
  tmp_ch_fls <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                          pattern = paste0("^MYD17A2.*", sds, "_1km.tif$"))
  
  tmp_rst_crp <- foreach(i = tmp_ch_fls, .combine = "stack") %do% {
    tmp_rst <- raster(i)
    
    crop(tmp_rst, rst_tmp, format = "GTiff", overwrite = TRUE,
         filename = paste0(ch_dir_myd17, "/crp/CRP_", basename(i)))
  }
  
  return(tmp_rst_crp)
}

rst_gpp <- ls_myd17_crp[[1]] * 0.0001
rst_qc <- ls_myd17_crp[[2]]

# qc
# rst_myd17_crp_qc <- foreach(i = 1:nlayers(rst_gpp), j = names(rst_gpp),
#                             .packages = c("rgdal", "raster"), 
#                             .combine = "stack") %dopar% {
#   overlay(rst_gpp[[i]], rst_qc[[i]], fun = function(x, y) {
#     num_x <- x[]
#     num_y <- y[]
#     num_y_qc <- sapply(num_y, qcMyd17)
#     log_y_qc <- is.na(num_y_qc)
#     num_x[log_y_qc] <- NA
#     return(num_x)
#   }, filename = paste0(ch_dir_myd17, "/qc/QC_", j), format = "GTiff", 
#   overwrite = TRUE)
# }

fls_myd17_crp_qc <- list.files(ch_dir_myd17, pattern = "^QC", 
                               full.names = TRUE, recursive = TRUE)
rst_myd17_crp_qc <- stack(fls_myd17_crp_qc)
mat_myd17_crp_qc <- as.matrix(rst_myd17_crp_qc)

# tsOutliers
# rst_myd17_crp_qc_tso <- calc(rst_myd17_crp_qc, fun = function(x) {
#   tmp_num <- as.numeric(x)
#   
#   if (!all(is.na(tmp_num))) {
#     tmp_int_id <- tsOutliers(tmp_num, lower_quantile = .4, upper_quantile = .9, 
#                              index = TRUE)
#     if (length(tmp_int_id) > 0)
#       tmp_num[tmp_int_id] <- NA
#   }
#   
#   return(tmp_num)
# }, filename = paste0(ch_dir_myd17, "/tso/TSO"), bylayer = TRUE, 
# suffix = names(rst_myd17_crp_qc), format = "GTiff", overwrite = TRUE)

fls_myd17_crp_qc_tso <- list.files(ch_dir_myd17, pattern = "^TSO", 
                                   full.names = TRUE, recursive = TRUE)
rst_myd17_crp_qc_tso <- stack(fls_myd17_crp_qc_tso)

# gap-filling
# rst_myd17_crp_qc_gf <- calc(rst_myd17_crp_qc_tso, fun = function(i) {
#   tmp_num_gpp <- i
#   tmp_log_isna <- is.na(tmp_num_gpp)
#   
#   tmp_num_mav <- rollapply(tmp_num_gpp, width = 3, fill = NA, 
#                            FUN = function(...) mean(..., na.rm = TRUE))
#   
#   tmp_num_gpp[tmp_log_isna] <- tmp_num_mav[tmp_log_isna]
#   return(tmp_num_gpp)
# }, filename = paste0(ch_dir_myd17, "/gf/GF"), bylayer = TRUE, 
# suffix = names(rst_myd17_crp_qc_tso), format = "GTiff", overwrite = TRUE)

fls_myd17_crp_qc_tso_gf <- list.files(ch_dir_myd17, pattern = "^GF_TSO", 
                                      full.names = TRUE, recursive = TRUE)
rst_myd17_crp_qc_tso_gf <- stack(fls_myd17_crp_qc_tso_gf)
mat_myd17_crp_qc_tso_gf <- as.matrix(rst_myd17_crp_qc_tso_gf)

# plot extraction (3-by-3 matrix)
spt_crd <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd)
spt_crd_amp <- subset(spt_crd, PoleType == "AMP")

ls_sls_gpp <- lapply(1:nrow(df_sls_fls), function(i) {
  tmp_spt_crd_amp <- subset(spt_crd_amp, PlotID == df_sls_fls$plot[i])
  tmp_int_crd_px <- cellFromXY(rst_myd17_crp_qc_tso_gf, tmp_spt_crd_amp)
  tmp_int_crd_px_adj <- adjacent(rst_myd17_crp_qc_tso_gf, tmp_int_crd_px, 
                                 sorted = TRUE, directions = 8, include = TRUE, 
                                 pairs = FALSE)
  tmp_mat_gpp <- mat_myd17_crp_qc_tso_gf[tmp_int_crd_px_adj, ]
  tmp_df_gpp <- data.frame(tmp_mat_gpp)
  
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df_gpp)
})
df_sls_gpp <- do.call("rbind", ls_sls_gpp)

# extraction of temporal range per plot
df_sls_tmp_rng <- slsTemporalRange(df_sls_fls)

ls_sls_gpp_md <- lapply(1:nrow(df_sls_tmp_rng), function(i) {
  # subset gpp data by current plot and season
  tmp_df_gpp <- subset(df_sls_gpp, plot == df_sls_tmp_rng$plot[i] &
                         season == df_sls_tmp_rng$season[i])
  
  # identify gpp scene(s) corresponding to sls measurement
  tmp_df_sls_rng <- subset(df_sls_tmp_rng, plot == df_sls_tmp_rng$plot[i] &
                             season == df_sls_tmp_rng$season[i])
  tmp_int_id_gpp <- slsGppIndex(df_sls = tmp_df_sls_rng, 
                                dt_fls_gpp = dt_fls_gpp, offset = 2)
  
  tmp_df_sls_gpp <- data.frame(tmp_df_sls_rng, tmp_df_gpp[, tmp_int_id_gpp])
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(tmp_df_sls_gpp) > 5) {
    tmp_df <- tmp_df_sls_gpp[5:ncol(tmp_df_sls_gpp)]
    tmp_num_mu <- rowMeans(tmp_df, na.rm = TRUE)
    tmp_df_sls_gpp <- data.frame(tmp_df_sls_gpp[, 1:4], tmp_num_mu)
  }
  
  data.frame(tmp_df_sls_gpp[1, 1:4], 
             gpp = median(tmp_df_sls_gpp[, 5], na.rm = TRUE))
})
df_sls_gpp_md <- do.call("rbind", ls_sls_gpp_md)
save("df_sls_gpp_md", file = paste0(ch_dir_out_agg01d, "df_sls_gpp_md.RData"))

# deregister parallel backend
closeAllConnections()
