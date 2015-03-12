slsTemporalRange <- function(df_fls) {
  
  ls_sls_tmp_rng <- lapply(1:nrow(df_fls), function(i) {
    tmp_fls <- df_fls[i, "mrg"]
    tmp_df <- read.csv(tmp_fls, stringsAsFactors = FALSE)
    tmp_df$datetime <- substr(tmp_df$datetime, 1, 10)
    
    tmp_dt_st <- as.Date(tmp_df[1, "datetime"])
    tmp_dt_nd <- as.Date(tmp_df[nrow(tmp_df), "datetime"])
    
    data.frame(plot = df_fls$plot[i], season = df_fls$season[i], 
               begin = tmp_dt_st, end = tmp_dt_nd)
  })
  
  df_sls_tmp_rng <- do.call("rbind", ls_sls_tmp_rng)
  return(df_sls_tmp_rng)
}