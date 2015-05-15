mergeGPX <- function(df2sp = FALSE, ...) {
  
  # packages
  stopifnot(require(plotKML))
  stopifnot(require(plyr))
  
  # os-dependent path to 'permanent' partition
  ch_dir_os <- switch(Sys.info()[["sysname"]], 
                      "Linux" = "/media/permanent/", 
                      "Windows" = "C:/Permanent/")
  
  ## file #1
  ch_dir1 <- paste0(ch_dir_os, "kilimanjaro/coordinates/coords/")
  ch_fls1 <- "sp01_gps_coordinates.gpx"
  ls_gps1 <- readGPX(paste0(ch_dir1, ch_fls1))
  df_gps1 <- ls_gps1$waypoints
  
  int_id_gps1_t <- grep("-T$", df_gps1$name)
  int_id_gps1_r <- grep("-R$", df_gps1$name)
  
  ls_gps1_tr <- lapply(c(int_id_gps1_t, int_id_gps1_r), function(i) {
    df_gps1[i, ]
  })
  df_gps1_tr <- do.call("rbind", ls_gps1_tr)
  
  ch_plt_tr <- df_gps1_tr$name
  ch_plt <- substr(ch_plt_tr, 1, 4)
  ch_tr <- substr(ch_plt_tr, 6, 6)
  
  df_gps1_tr$plot <- ch_plt
  df_gps1_tr$unit <- ch_tr
  
  ## file #2
  ch_dir2 <- paste0(ch_dir_os, "phd/gps/waypoints")
  ch_fls2 <- list.files(ch_dir2, full.names = TRUE)
  
  ls_gps2_tr <- lapply(ch_fls2, function(i) {
    tmp_ls_gps <- readGPX(i, tracks = FALSE, routes = FALSE)
    tmp_df_gps <- tmp_ls_gps$waypoints
    
    if (is.null(tmp_df_gps)) {
      return(NA)
    } else {
      
      tmp_int_id_gps2_t <- grep("-T$", tmp_df_gps$cmt)
      tmp_int_id_gps2_r <- grep("-R$", tmp_df_gps$cmt)

      tmp_int_id_gps2_td <- grep("-T-D$", tmp_df_gps$cmt)
      tmp_int_id_gps2_rd <- grep("-R-D$", tmp_df_gps$cmt)
      
      tmp_int_id_gps2_t <- c(tmp_int_id_gps2_t, tmp_int_id_gps2_td)
      tmp_int_id_gps2_r <- c(tmp_int_id_gps2_r, tmp_int_id_gps2_rd)
      
      tmp_ls_gps2_tr <- lapply(c(tmp_int_id_gps2_t, tmp_int_id_gps2_r), function(i) {
        tmp_df_gps[i, ]
      })
      tmp_df_gps2_tr <- do.call("rbind", tmp_ls_gps2_tr)
      
      tmp_ch_plt_tr <- tmp_df_gps2_tr$cmt
      ch_plt <- substr(tmp_ch_plt_tr, 1, 4)
      ch_tr <- substr(tmp_ch_plt_tr, 6, 6)
      
      ch_ssn <- sapply(tmp_ch_plt_tr, function(j) {
        if (nchar(j) > 6) tmp_ch_ssn <- substr(j, 8, 8) else tmp_ch_ssn <- NA
        return(tmp_ch_ssn)
      })
      
      tmp_df_gps2_tr$plot <- ch_plt
      tmp_df_gps2_tr$unit <- ch_tr
      tmp_df_gps2_tr$season <- ch_ssn
      
      return(tmp_df_gps2_tr)
    }
  })
  
  bool_navl <- sapply(ls_gps2_tr, function(i) {
    tmp_bool_isnull <- is.null(i)
    if (tmp_bool_isnull) {
      return(TRUE)
    } else {
      tmp_bool_isna <- is.na(i)
      if (tmp_bool_isna) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  })
  
  ls_gps2_tr <- ls_gps2_tr[!bool_navl]
  df_gps2_tr <- do.call("rbind", ls_gps2_tr)
 
  ## file #3
  ch_dir3 <- paste0(ch_dir_os, "phd/gps/waypoints/")
  ch_fls3 <- "Waypoints_01-DEC-14.gpx"
  ls_gps3 <- readGPX(paste0(ch_dir3, ch_fls3))
  df_gps3 <- ls_gps3$waypoints
  
  int_id_gps3_t <- grep("-T$", df_gps3$name)
  int_id_gps3_r <- grep("-R$", df_gps3$name)
  
  ls_gps3_tr <- lapply(c(int_id_gps3_t, int_id_gps3_r), function(i) {
    df_gps3[i, ]
  })
  df_gps3_tr <- do.call("rbind", ls_gps3_tr)
  
  ch_plt_tr <- df_gps3_tr$name
  ch_plt <- substr(ch_plt_tr, 1, 4)
  ch_tr <- substr(ch_plt_tr, 6, 6)
  
  df_gps3_tr$plot <- ch_plt
  df_gps3_tr$unit <- ch_tr
  
  
  ## merge gpx data
  df_gps <- rbind.fill(df_gps1_tr, df_gps2_tr, df_gps3_tr)
  
  
  ## no dry season -> wet season
  bool_isna_ssn <- is.na(df_gps$season)
  df_gps$season[bool_isna_ssn] <- "W"
  
  ## return spatial object (optional)
  if (df2sp) {
    stopifnot(require(raster))
    sp_gps <- df_gps
    coordinates(sp_gps) <-  ~ lon + lat
    projection(sp_gps) <- "+init=epsg:4326"
    return(sp_gps)
  } else {
    return(df_gps)
  }
}