slsAggregate <- function(fn, agg_by = 20, include_time = TRUE, ...) {  
  
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
    tmp_agg_et <- rollapply(tmp.dat$waterET, width = agg_by, by = agg_by, ...)
    tmp_df_agg <- data.frame(datetime = tmp_agg_datetime, waterET = tmp_agg_et)
  }
  
  # add time column (optional)
  if (include_time) {
    tmp_df_agg$time <- sapply(strsplit(as.character(tmp_df_agg$datetime), " "), "[[", 2)
    tmp_df_agg$time <- as.factor(tmp_df_agg$time)
  }
  
  return(tmp_df_agg)
}