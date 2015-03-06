mergeGPX <- function(...) {
  
  stopifnot(require(plotKML))
  stopifnot(require(plyr))
  
  ## file #1
  ch_dir1 <- "/media/permanent/kilimanjaro/coordinates/coords/"
  ch_fls1 <- "sp01_gps_coordinates.gpx"
  ls_gps1 <- readGPX(paste0(ch_dir1, ch_fls1))
  df_gps1 <- ls_gps1$waypoints
  
  int_id_gps1_t <- grep("-T$", df_gps1$name)
  int_id_gps1_r <- grep("-R$", df_gps1$name)
  
  ls_gps1_tr <- lapply(c(int_id_gps1_t, int_id_gps1_r), function(i) {
    df_gps1[i, ]
  })
  df_gps1_tr <- do.call("rbind", ls_gps1_tr)
  
  
  ## file #2
  ch_dir2 <- "/media/permanent/phd/gps/waypoints"
  ch_fls2 <- list.files(ch_dir2, full.names = TRUE)
  
  ls_gps2_tr <- lapply(ch_fls2, function(i) {
    tmp_ls_gps <- readGPX(i, tracks = FALSE, routes = FALSE)
    tmp_df_gps <- tmp_ls_gps$waypoints
    
    if (is.null(tmp_df_gps)) {
      return(NA)
    } else {
      
      tmp_int_id_gps2_t <- grep("-T$", tmp_df_gps$cmt)
      tmp_int_id_gps2_r <- grep("-R$", tmp_df_gps$cmt)
      
      tmp_ls_gps2_tr <- lapply(c(tmp_int_id_gps2_t, tmp_int_id_gps2_r), function(i) {
        tmp_df_gps[i, ]
      })
      tmp_df_gps2_tr <- do.call("rbind", tmp_ls_gps2_tr)
      
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
 
  
  ## merge gpx data
  df_gps <- rbind.fill(df_gps1_tr, df_gps2_tr)
  return(df_gps)
}