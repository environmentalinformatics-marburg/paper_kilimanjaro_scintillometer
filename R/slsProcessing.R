slsProcessing <- function(sls_wsp, rf_var = NULL, dsn = ".", fill = TRUE, 
                          agg = TRUE, agg_int = "hourly") {
  
  # packages
  stopifnot(require(randomForest))
  
  # functions
  source("R/slsMergeDailyData.R")
  
  ch_wsp_sh <- basename(sls_wsp)
  ls_wsp_sh <- strsplit(ch_wsp_sh, "_")
  ch_plt <- sapply(ls_wsp_sh, "[[", 3)
  
  dsn <- paste(dsn, basename(sls_wsp), sep = "/")
  
  
  ## step 1: merge daily data
  
  tmp_ch_fls <- list.files(paste0(sls_wsp, "/data/retrieved_SPU-111-230"), 
                           pattern = ".mnd$", full.names = TRUE)
  
  tmp_df <- slsMergeDailyData(files = tmp_ch_fls)
  
  
  ## step 2: selected columns
  
  if (is.null(rf_var))
    rf_var <- c("datetime", "tempUp", "tempLw", "dwnRad", "upwRad", 
                "humidity", "soilHeatFlux", "pressure", 
                "precipRate", "waterET")
  
  tmp_df2 <- tmp_df[, rf_var]
  tmp_df2$datetime <- strptime(tmp_df2$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  # output storage
  if (!file.exists(dsn)) 
    dir.create(dsn)
  
  write.csv(tmp_df2, paste0(dsn, "/", ch_plt, "_mrg.csv"), row.names = FALSE)
  
  
  ## step 3: gap-filling
  
  if (fill) {
    bool_cc <- complete.cases(tmp_df2[, -1])
    
    tmp_df2_train <- tmp_df2[bool_cc, 2:ncol(tmp_df2)]
    tmp_df2_test <- tmp_df2[!bool_cc, ]
    
    tmp_df2_rf <- randomForest(waterET ~ ., data = tmp_df2_train)
    
    # Random forest-based ET prediction
    pred <- predict(tmp_df2_rf, tmp_df2_test)
    tmp_df3 <- tmp_df2
    tmp_df3[!bool_cc, "waterET"] <- pred
    
    write.csv(tmp_df3, paste0(dsn, "/", ch_plt, "_mrg_rf.csv"), row.names = FALSE)
  } else {
    tmp_df3 <- tmp_df2
  }
  
  # 1h aggregation  
  if (agg) {
    if (agg_int == "hourly") {
      # hourly aggregation
      tmp_df4 <- aggregate(tmp_df3[, 2:ncol(tmp_df3)], by = list(substr(tmp_df3[, 1], 1, 13)), 
                           FUN = function(x) round(mean(x, na.rm = TRUE), 2))
      tmp_df4[, 1] <- paste0(tmp_df4[, 1], ":00")
      names(tmp_df4)[1] <- "datetime"
      tmp_df4$datetime <- strptime(tmp_df4$datetime, format = "%Y-%m-%d %H:%M")
      
      write.csv(tmp_df4, paste0(dsn, "/", ch_plt, "_mrg_rf_agg01h.csv"), 
                row.names = FALSE)
    } else {
      
      # 10m aggregation
      tmp_df4 <- aggregate(tmp_df3[, 2:ncol(tmp_df3)], by = list(substr(tmp_df3[, 1], 1, 15)), 
                           FUN = function(x) round(mean(x, na.rm = TRUE), 2))
      tmp_df4[, 1] <- paste0(tmp_df4[, 1], "0")
      names(tmp_df4)[1] <- "datetime"
      
      write.csv(tmp_df4, paste0(dsn, "/", ch_plt, "_mrg_rf_agg10m.csv"), 
                row.names = FALSE)
    }
  } else {
    tmp_df4 <- tmp_df3
  }  
  
  return(tmp_df4)
}