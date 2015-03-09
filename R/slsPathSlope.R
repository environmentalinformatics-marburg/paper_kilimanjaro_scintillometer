slsPathSlope <- function(dat, sp_dat, dem, ...) {
  
  stopifnot(require(gmt))
  
  mat_gps_plt_ssn <- cbind(dat$plot, dat$season)
  mat_gps_plt_ssn <- unique(mat_gps_plt_ssn)
 
  ls_slp <- lapply(1:nrow(mat_gps_plt_ssn), function(i) {
    
    tmp_ch_plt <- mat_gps_plt_ssn[i, 1]
    tmp_ch_ssn <- mat_gps_plt_ssn[i, 2]
    
    tmp_df_gps <- subset(dat, (plot == tmp_ch_plt) & (season == tmp_ch_ssn))
    
    int_id_t <- which(tmp_df_gps$unit == "T")
    int_id_r <- which(tmp_df_gps$unit == "R")
    
    if (!is.numeric(tmp_df_gps$ele))
      tmp_df_gps$ele <- as.numeric(tmp_df_gps$ele)
    
    # x (linear distance)
    Nfrom <- tmp_df_gps$lat[int_id_t]
    Efrom <- tmp_df_gps$lon[int_id_t]
    Nto <- tmp_df_gps$lat[int_id_r]
    Eto <- tmp_df_gps$lon[int_id_r]
    num_x <- geodist(Nfrom, Efrom, Nto, Eto)
    num_x <- num_x * 1000
    
    # h (level difference)
    num_h <- tmp_df_gps$ele[int_id_r] - tmp_df_gps$ele[int_id_t]
    
    # slope angle
    num_slp <- atan(num_h/num_x)
    num_slp <- num_slp * 180 / pi
    
    # slope angle from dem
    tmp_sp_gps <- subset(sp_dat, plot == tmp_ch_plt)
    int_id_t <- which(tmp_sp_gps$unit == "T")
    int_id_r <- which(tmp_sp_gps$unit == "R")
    tmp_num_sp_gps <- extract(dem, tmp_sp_gps)
    
    num_h_dem <- tmp_num_sp_gps[2] - tmp_num_sp_gps[1]
    num_slp_dem <- atan(num_h_dem/num_x)
    num_slp_dem <- num_slp_dem * 180 / pi
    
    tmp_df_slp <- data.frame(plot = tmp_ch_plt, beam_slope = num_slp, 
                             dem_slope = num_slp_dem)
    return(tmp_df_slp)
  })
 
  df_slp <- do.call("rbind", ls_slp)
  return(df_slp)
}