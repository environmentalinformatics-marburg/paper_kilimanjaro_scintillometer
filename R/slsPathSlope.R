slsPathSlope <- function(x, dem, ...) {
  
  stopifnot(require(gmt))
  
  mat_gps_plt_ssn <- data.frame(x)[, c("PlotID", "season", "unit", "lon", "lat", "ele")]
  mat_unq <- unique(mat_gps_plt_ssn[, c("PlotID", "season")])
 
  ls_slp <- lapply(1:nrow(mat_unq), function(i) {
    
    plt <- mat_unq[i, 1]; ssn <- mat_unq[i, 2]
    tmp_df_gps <- subset(mat_gps_plt_ssn, PlotID == plt & season == ssn)
    
    int_id_t <- which(tmp_df_gps$unit == "T")
    int_id_r <- which(tmp_df_gps$unit == "R")
    
    if (!is.numeric(tmp_df_gps$ele))
      tmp_df_gps$ele <- as.numeric(tmp_df_gps$ele)
    
    # x (linear distance)
    Nfrom <- tmp_df_gps$lat[int_id_t]
    Efrom <- tmp_df_gps$lon[int_id_t]
    Nto <- tmp_df_gps$lat[int_id_r]
    Eto <- tmp_df_gps$lon[int_id_r]
    
    asq <- (Eto - Efrom)^2
    bsq <- (Nto - Nfrom)^2
    num_x <- sqrt(asq + bsq)

    # h (level difference)
    num_h <- tmp_df_gps$ele[int_id_r] - tmp_df_gps$ele[int_id_t]
    
    # slope angle
    num_slp <- atan(num_h/num_x)
    num_slp <- num_slp * 180 / pi
    
    # slope angle from dem
    tmp_sp_gps <- subset(x, PlotID == plt)
    int_id_t <- which(tmp_sp_gps$unit == "T")
    int_id_r <- which(tmp_sp_gps$unit == "R")
    tmp_num_sp_gps <- extract(dem, tmp_sp_gps)
    
    num_h_dem <- tmp_num_sp_gps[2] - tmp_num_sp_gps[1]
    num_slp_dem <- atan(num_h_dem/num_x)
    num_slp_dem <- num_slp_dem * 180 / pi
    
    tmp_df_slp <- data.frame(PlotID = plt, season = ssn, inc_gps = num_slp, 
                             inc_dem = num_slp_dem)
    return(tmp_df_slp)
  })
 
  df_slp <- do.call("rbind", ls_slp)
  return(df_slp)
}