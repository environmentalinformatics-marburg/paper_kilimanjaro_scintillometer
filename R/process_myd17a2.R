## environmental stuff

# packages
library(MODIS)
library(Rsenal)
library(doParallel)

# parallelization
cl <- makeCluster(2)
registerDoParallel(cl)

# path: modis, raw and extracted
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

# path: modis, processed
ch_dir_myd17 <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/myd17a2/"

# modis options
MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)


## data

# # data download
# for (product in c("MOD17A2", "MYD17A2"))
#   runGdal(product, begin = "2014001", tileH = 21, tileV = 9, 
#           SDSstring = "101", job = "GPPkili", outProj = "+init=epsg:21037")

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
rst_myd17_crp_qc <- foreach(i = 1:nlayers(rst_gpp), j = names(rst_gpp),
                            .packages = c("rgdal", "raster"), 
                            .combine = "stack") %dopar% {
  overlay(rst_gpp[[i]], rst_qc[[i]], fun = function(x, y) {
    num_x <- x[]
    num_y <- y[]
    num_y_qc <- sapply(num_y, qcMyd17)
    log_y_qc <- is.na(num_y_qc)
    num_x[log_y_qc] <- NA
    return(num_x)
  }, filename = paste0(ch_dir_myd17, "/qc/QC_", j), format = "GTiff", 
  overwrite = TRUE)
}

closeAllConnections()