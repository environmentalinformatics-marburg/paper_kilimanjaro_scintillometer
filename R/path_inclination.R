## environmental stuff

# packages
library(plotKML)

# functions
source("R/mergeGPX.R")
source("R/spatialPointsToLines.R")

# gps data path and files
ch_dir_gps <- "/media/permanent/phd/gps/waypoints"
ch_fls_gps <- list.files(ch_dir_gps, full.names = TRUE)

# path and file with research plot coordinates, dem
ch_dir_dem <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_dem <- "DEM_ARC1960_30m_Hemp.tif"


## data

# plots included in sls field campaign
ch_plt_sls <- c("sav5", "sav4", 
                "mai4", "mai1", 
                "cof3", "cof2", 
                "gra1", "gra2", 
                "fer0", "fed1", 
                "hel1")

# gps data with scintillometer locations
df_gps <- mergeGPX()
sp_gps <- df_gps
coordinates(sp_gps) <-  ~ lon + lat
projection(sp_gps) <- "+init=epsg:4326"
sp_gps_clrk <- spTransform(sp_gps, CRS("+init=epsg:21037"))

# dem
rst_dem <- raster(paste0(ch_dir_crd, ch_fls_dem))
rst_slp <- terrain(rst_dem, unit = "degrees")


## slopes

ch_plt <- unique(sp_gps_clrk$plot)

ls_slp <- lapply(ch_plt, function(i) {
  sp_gps_clrk_sub <- subset(sp_gps_clrk, plot == i)
  # plot(sp_gps_clrk_sub, cex = 2)
  sl_gps_clrk_sub <- spatialPointsToLines(sp_gps_clrk_sub)
  # lines(sl_gps_clrk_sub, lty = 2, col = "grey50")
  
  tmp_num_slp <- extract(rst_slp, sl_gps_clrk_sub, fun = mean)
  tmp_df_slp <- data.frame(plot = i, slope = tmp_num_slp)
  
  return(tmp_df_slp)
})

df_slp <- do.call("rbind", ls_slp)


## propagation path inclination