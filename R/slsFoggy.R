slsFoggy <- function(channel_data, error_code = 32, date_col = c(1, 2), ...) {

  # errors indexes
  int_col_err <- grep("error", names(channel_data))
  log_id_err <- channel_data[, int_col_err] == error_code &
    !is.na(channel_data[, int_col_err])
  
  # reject measurements during fog events
  channel_data[log_id_err, -c(date_col, int_col_err)] <- NA
  
  return(channel_data)
}