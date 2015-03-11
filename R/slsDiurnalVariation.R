slsDiurnalVariation <- function(fn, plt, agg_by = 5) {
  
  # packages
  stopifnot(require(zoo))
  
  # Import current LUC
  tmp.dat <- read.csv(fn, stringsAsFactors = FALSE)
  tmp.dat <- tmp.dat[, c("datetime", "waterET")]
  
  # aggregation
  if (agg_by == 1) {
    tmp_df_agg <- tmp.dat
  } else {
    tmp_agg_datetime <- tmp.dat$datetime[seq(1, nrow(tmp.dat), agg_by)]
    tmp_agg_et <- rollapply(tmp.dat$waterET, width = agg_by, by = agg_by, 
                            FUN = function(...) mean(..., na.rm = TRUE))
    tmp_df_agg <- data.frame(datetime = tmp_agg_datetime, waterET = tmp_agg_et, 
                             stringsAsFactors = FALSE)
  }
  
  # Split by day
  tmp_df_agg$time <- substr(tmp_df_agg$datetime, 12, 19)
  tmp_df_agg$date <- substr(tmp_df_agg$datetime, 1, 10)
  tmp_df_agg_split <- split(tmp_df_agg, tmp_df_agg$date)
  
  # Merge by minute of day
  for (i in 1:length(tmp_df_agg_split)) {
    if (i == 1) {
      tmp_dat_mrg <- tmp_df_agg_split[[i]]
      tmp_dat_mrg <- tmp_dat_mrg[, -grep("date", names(tmp_dat_mrg))]
      tmp_dat_mrg <- tmp_dat_mrg[, c(2, 1)]
      names(tmp_dat_mrg)[i+1] <- paste0("waterET_", names(tmp_df_agg_split)[i])
    } else {
      tmp_dat_mrg <- merge(tmp_dat_mrg, tmp_df_agg_split[[i]], by = "time")
      tmp_dat_mrg <- tmp_dat_mrg[, -grep("date", names(tmp_dat_mrg))]
      names(tmp_dat_mrg)[i+1] <- paste0("waterET_", names(tmp_df_agg_split)[i])
    }
  }
  
  # Return rowMedians per minute of day
  tmp_mat_mrg <- as.matrix(tmp.dat.mrg[, 2:ncol(tmp.dat.mrg)])
  tmp_med_mrg <- apply(tmp_mat_mrg, 1, 
                       FUN = function(...) median(..., na.rm = TRUE))
  tmp_df_mrg <- data.frame(plot = plt, 
                           time = tmp_dat_mrg$time,
                           waterET = tmp_med_mrg)
  return(tmp_df_mrg)
}