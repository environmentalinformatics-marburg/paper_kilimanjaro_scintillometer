slsFoggy <- function(channel_data, use_error = TRUE, error_code = 32, 
                     threshold = NULL, probs = .1, date_col = c(1, 2), ...) {

  int_col_err <- grep("error", names(channel_data))
  
  if (use_error) {
    # errors indexes
    log_id_err <- channel_data[, int_col_err] == error_code &
      !is.na(channel_data[, int_col_err])
    
    # reject measurements during fog events
    channel_data[log_id_err, -c(date_col, int_col_err)] <- NA
    
  } else {
    # rejection based on quantile limit or fixed threshold
    ch_channels <- c("<XA", "<YA", "<XB", "<YB")
    int_col_channels <- sapply(ch_channels, function(i) {
      grep(i, names(channel_data))
    })
    df_channels <- channel_data[, int_col_channels]
    
    if (is.null(threshold)) {
      # signal quantiles
      num_channels_quan <- apply(df_channels, 2, FUN = function(...) {
        quantile(..., probs = probs, na.rm = TRUE)
      })
      
      # rejection of measurement if at least one channel drops below calculated
      # quantile threshold
      ls_channels_srd <- lapply(1:ncol(df_channels), function(i) {
        df_channels[, i] < num_channels_quan[i] &
          !is.na(df_channels[, i])
      })
      mat_channels_srd <- do.call("cbind", ls_channels_srd)
      log_channels_srd <- apply(mat_channels_srd, 1, FUN = any)
      
      channel_data[log_channels_srd, -c(date_col, int_col_err)] <- NA
      
    } else {
      # rejection of measurement if at least one channel dropbs below
      # fixed threshold
      ls_channels_srd <- lapply(1:ncol(df_channels), function(i) {
        df_channels[, i] < threshold &
          !is.na(df_channels[, i])
      })
      mat_channels_srd <- do.call("cbind", ls_channels_srd)
      log_channels_srd <- apply(mat_channels_srd, 1, FUN = any)
      
      channel_data[log_channels_srd, -c(date_col, int_col_err)] <- NA
    }    
  }
  
  return(channel_data)
}
  