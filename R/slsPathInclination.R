slsPathInclination <- function(digits = 1L, ...) {
  
  ## environmental stuff
  
  # packages
  library(plotKML)
  library(raster)
  
  # functions
  source("R/mergeGPX.R")
  source("R/spatialPointsToLines.R")
  source("R/slsPathSlope.R")
  
  # path: coordinates and dem
  ch_dir_crd <- "../../kilimanjaro/coordinates/"
  ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

  ch_fls_dem <- "DEM_ARC1960_30m_Hemp.tif"
  
  # gps data path and files
  ch_dir_gps <- "/media/permanent/phd/gps/waypoints"
  ch_fls_gps <- list.files(ch_dir_gps, full.names = TRUE)
  

  ## data
  
  # gps data with scintillometer locations
  sp_gps <- readRDS("data/sensors.rds")
  sp_gps_clrk <- spTransform(sp_gps, CRS("+init=epsg:21037"))
  
  # dem
  rst_dem <- raster(paste0(ch_dir_crd, ch_fls_dem))
  rst_slp <- terrain(rst_dem, unit = "degrees")
  
  
  ## terrain slopes, i.e. average slope between transmitter and 
  ## receiver unit, calculated from dem
  ch_plt <- unique(sp_gps_clrk$PlotID)
  unq <- unique(sp_gps_clrk@data[c("PlotID", "season")])
  
  ls_slp <- lapply(1:nrow(unq), function(i) {
    plt <- unq$PlotID[i]; ssn <- unq$season[i]
    sp_gps_clrk_sub <- subset(sp_gps_clrk, PlotID == plt & season == ssn)
    # plot(sp_gps_clrk_sub, cex = 2)
    sl_gps_clrk_sub <- spatialPointsToLines(sp_gps_clrk_sub)
    # lines(sl_gps_clrk_sub, lty = 2, col = "grey50")
    
    tmp_num_slp <- extract(rst_slp, sl_gps_clrk_sub, fun = mean)
    tmp_df_slp <- data.frame(PlotID = plt, season = ssn, 
                             slope = round(tmp_num_slp, digits))
    
    return(tmp_df_slp)
  })
  
  df_slp <- do.call("rbind", ls_slp)
  
  
  ## beam path inclination
  
  df_slp_bp <- slsPathSlope(sp_gps_clrk, rst_dem)
  df_slp_bp$inc_gps <- round(df_slp_bp$inc_gps, digits)
  df_slp_bp$inc_dem <- round(df_slp_bp$inc_dem, digits)
  
  # correction of false inclinations
  int_id_sav0 <- grep("sav0", df_slp_bp$PlotID)
  df_slp_bp$inc_gps[int_id_sav0[2]] <- df_slp_bp$inc_gps[int_id_sav0[1]]
  
  int_id_sav5 <- grep("sav5", df_slp_bp$PlotID)
  df_slp_bp$inc_gps[int_id_sav5[2]] <- df_slp_bp$inc_gps[int_id_sav5[1]]
  
  int_id_mai4 <- grep("mai4", df_slp_bp$PlotID)
  df_slp_bp$inc_gps[int_id_mai4] <- df_slp_bp$inc_dem[int_id_mai4]

  int_id_mai0 <- grep("mai0", df_slp_bp$PlotID)
  df_slp_bp$inc_gps[int_id_mai0] <- df_slp_bp$inc_dem[int_id_mai0]
  
  int_id_gra2 <- grep("gra2", df_slp_bp$PlotID)
  df_slp_bp$inc_gps[int_id_gra2] <- df_slp_bp$inc_dem[int_id_gra2]
  
  df_slp_bp <- df_slp_bp[, 1:3]
  names(df_slp_bp)[3] <- "inclination"
  
  ## merge data
  
  ls_slp_terr_bp <- list(df_slp, df_slp_bp)
  df_slp_terr_bp <- do.call(function(...) merge(..., by = c("PlotID", "season"), 
                                                all = TRUE), ls_slp_terr_bp)
  
  return(df_slp_terr_bp)
}