slsAggregate <- function(fn, agg_by = 20, include_time = TRUE, ...) {  
  
  # packages
  stopifnot(require(zoo))
  
  # Import current LUC
  if (is.character(fn)) {
    tmp.dat <- read.csv(fn, stringsAsFactors = FALSE)
  } else {
    tmp.dat <- fn
  }
  
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
    tmp_ls_agg <- strsplit(as.character(tmp_df_agg$datetime), " ")
    tmp_ls_agg_len <- sapply(tmp_ls_agg, length)
    if (tmp_ls_agg_len > 1) {
      tmp_df_agg$time <- sapply(tmp_ls_agg, "[[", 2)
    } else {
      tmp_df_agg$time <- sapply(tmp_ls_agg, "[[", 1)
    }
    tmp_df_agg$time <- as.factor(tmp_df_agg$time)
  }
  
  return(tmp_df_agg)
}