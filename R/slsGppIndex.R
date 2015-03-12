slsGppIndex <- function(df_sls, dt_fls_gpp, offset = NULL) {
  
  # starting and end date of sls measurement
  tmp_df_sls_st <- df_sls$begin
  tmp_df_sls_nd <- df_sls$end
  
  # daily sequence from 8-day gpp files
  ls_dt_sq <- lapply(dt_fls_gpp, function(i) {
    tmp_dt_sq <- seq(i, i+7, "day")
    data.frame(date = i, date_incl = tmp_dt_sq)
  })
  df_dt_sq <- do.call("rbind", ls_dt_sq)
  
  # identify in which 8-day gpp file(s) the sls measurement is included
  tmp_df_dt_sq <- subset(df_dt_sq, 
                         date_incl == tmp_df_sls_st | date_incl == tmp_df_sls_nd)
  
  tmp_dt_uq <- unique(tmp_df_dt_sq$date)
  if (length(tmp_dt_uq) == 1) {
    tmp_int_sls_rng_id <- which(dt_fls_gpp == tmp_dt_uq)
  } else {
    tmp_int_sls_rng_id <- sapply(tmp_dt_uq, function(i) {
      which(dt_fls_gpp == i)
    })
  }
  
  # add offset
  if (!is.null(offset))
    tmp_int_sls_rng_id <- tmp_int_sls_rng_id + offset
  
  return(tmp_int_sls_rng_id)
}