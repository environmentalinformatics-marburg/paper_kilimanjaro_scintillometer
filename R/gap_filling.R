## environmental stuff

# packages
lib <- c("caret", "randomForest", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# functions
source("R/slsMergeDailyData.R")

# parallelization
cl <- makeCluster(4)
registerDoParallel(cl)

# path: srun
ch_dir_srun <- "../../SRun/"


## data processing
srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# training control parameters
fit_control <- trainControl(method = "cv", number = 10, repeats = 10, 
                            verboseIter = TRUE)

# gap-filling
ls_sls_rf <- lapply(srunWorkspaces, function(i) {
  
  tmp_ls_plt <- strsplit(basename(i), "_")
  tmp_ch_plt <- sapply(tmp_ls_plt, "[[", 3)
  
  tmp_fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), 
                        pattern = ".mnd$", full.names = TRUE)
  
  # Merge daily .mnd files
  tmp_df <- slsMergeDailyData(files = tmp_fls)
  # Subset columns relevant for randomForest algorithm
  tmp_df_sub <- tmp_df[, c("datetime", 
                           "tempUp", "tempLw", "dwnRad", "upwRad", "humidity",
                           "soilHeatFlux", "pressure", "precipRate", "waterET")]
  
  # complete meteorological records
  tmp_df_sub_prd <- tmp_df_sub[, 2:(ncol(tmp_df_sub)-1)]
  tmp_id_sub_prd_cc <- complete.cases(tmp_df_sub_prd)
  
  # missing et recordings
  tmp_df_sub_rsp <- tmp_df_sub[, ncol(tmp_df_sub)]
  tmp_id_sub_rsp_na <- is.na(tmp_df_sub_rsp)
  
  # indices of training data (complete meteorological/et observations) 
  # and newdata (complete meteorological/missing et observations)
  tmp_int_id_trn <- which(tmp_id_sub_prd_cc & !tmp_id_sub_rsp_na)
  tmp_df_sub_trn <- tmp_df_sub[tmp_int_id_trn, -1]

  tmp_int_id_tst <- which(tmp_id_sub_prd_cc & tmp_id_sub_rsp_na)
  tmp_df_sub_tst <- tmp_df_sub[tmp_int_id_tst, -1]

  # random forest
  tmp_rf <- train(waterET ~ ., data = tmp_df_sub_trn, method = "rf", 
                  trControl = fit_control)
  
  # random forest-based ET prediction
  tmp_prd <- predict(tmp_rf, tmp_df_sub_tst)
  
  # gap-filling
  tmp_df_sub_gf <- tmp_df_sub
  tmp_df_sub_gf[tmp_int_id_tst, "waterET"] <- tmp_prd
  
  # output storage
  tmp_ch_dir_out <- paste0("../../phd/scintillometer/data/sls/", basename(i))
  tmp_ch_fls_out <- paste0(tmp_ch_plt, "_mrg_rf.csv")
  write.csv(tmp_df_sub_gf, paste(tmp_ch_dir_out, tmp_ch_fls_out, sep = "/"), 
            quote = FALSE, row.names = FALSE)
  
  #   dat3 <- dat2
  #   dat3[index, "waterET"] <- pred
  #   dat3$waterET <- as.numeric(as.character(dat3$waterET))
  #   
  #   write.csv(dat3, paste0(i, "/data/out/", plt, "_mrg_rf.csv"), row.names = FALSE)
  #   
  #   # 1h aggregation  
  #   dat4 <- aggregate(dat3[, 2:ncol(dat3)], by = list(substr(dat3[, 1], 1, 13)), 
  #                     FUN = function(x) round(mean(x, na.rm = TRUE), 1))
  #   dat4[, 1] <- paste0(dat4[, 1], ":00")
  #   names(dat4)[1] <- "datetime"
  #   dat4$datetime <- strptime(dat4$datetime, format = "%Y-%m-%d %H:%M")
  #   
  #   
  #   write.csv(dat4, paste0(i, "/data/out/", plt, "_mrg_rf_agg01h.csv"), 
  #             row.names = FALSE)
  #   
  #   # 10m aggregation
  #   dat5 <- aggregate(tmp_df_sub_gf[, 2:ncol(tmp_df_sub_gf)], 
  #                     by = list(substr(tmp_df_sub_gf[, 1], 1, 15)), 
  #                     FUN = function(x) round(mean(x, na.rm = TRUE), 1))
  #   dat5[, 1] <- paste0(dat5[, 1], "0")
  #   names(dat5)[1] <- "datetime"
  #   dat5$datetime <- strptime(dat5$datetime, format = "%Y-%m-%d %H:%M")
  
  #   write.csv(dat5, paste0(i, "/data/out/", plt, "_mrg_rf_agg10m.csv"), 
  #             row.names = FALSE)
    
  #   ggplot(aes(x = strptime(datetime, format = "%Y-%m-%d %H:%M"), y = waterET), 
  #          data = tmp_df_sub) + 
  #     geom_line(colour = "grey65") + 
  #     geom_point(colour = "grey65") + 
  #     geom_point(aes(x = datetime, y = waterET), data = dat5) + 
  #     geom_hline(aes(y = 0), colour = "darkgrey") + 
  #     labs(x = "Time [h]", y = "Evapotranspiration [mm/h]") + 
  #     theme_bw()
  
  return(tmp_df_sub_gf)
})

# deregister parallel backend
closeAllConnections()