### environmental stuff -----

## clear working directory
rm(list = ls(all = TRUE))

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

## path: srun
dir_srun <- "../../SRun/"


### data processing

srunWorkspaces <- dir(dir_srun, pattern = "^workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# training control parameters
fit_control <- trainControl(method = "cv", number = 10, repeats = 10, 
                            verboseIter = TRUE)

# variables relevant for rf procedure
var_rf <- c("tempUp", "netRad", "humidity", "soilHeatFlux", "pressure", 
            "precipRate", "waterET")

ls_rf_scores_whr <- lapply(srunWorkspaces, function(i) {
  
  ## status message
  cat("Commencing with the processing of", i, "\n")
  
  tmp_ls_plt <- strsplit(basename(i), "_")
  tmp_plt <- sapply(tmp_ls_plt, "[[", 3)
  tmp_dt <- sapply(tmp_ls_plt, "[[", 4)
  
  # tmp_fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), 
  #                       pattern = ".mnd$", full.names = TRUE)
  drs <- dir(paste0(i, "/data"), pattern = "^Reprocessed", full.names = TRUE)
  tmp_fls <- do.call("c", lapply(drs, function(j) {
    list.files(j, pattern = ".mnd$", full.names = TRUE)
  }))

  ## merge daily files
  tmp_df <- slsMergeDailyData(files = tmp_fls)
  tmp_df$datetime <- strptime(tmp_df$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  ## eliminate "N/A" strings introduced during data reprocessing
  tmp_df$waterET <- suppressWarnings(as.numeric(tmp_df$waterET))
  
  ## adjust et rates < 0
  num_et <- tmp_df[, "waterET"]
  log_st0 <- which(!is.na(num_et) & num_et < 0)
  if (length(log_st0) > 0)
    tmp_df[log_st0, "waterET"] <- 0
  
  # fog events (fer, fed and hel only)
  if (tmp_plt %in% c("fer0", "fed1", "hel1")) {
    tmp_df_fog <- slsFoggy(tmp_df, use_error = FALSE, probs = .1)
    tmp_df[tmp_df_fog$fog, var_rf] <- NA
  }

  ## fill mai4 nighttime et
  if (tmp_plt == "mai4") {
    date1 <- as.POSIXct("2014-05-13 22:00:00")
    date2 <- as.POSIXct("2014-05-14 05:59:00")
    int <- interval(date1, date2)
    tmp_df[tmp_df$datetime %within% int, "waterET"] <- 0
    
    date1 <- as.POSIXct("2014-05-15 22:00:00")
    date2 <- as.POSIXct("2014-05-16 05:59:00")
    int <- interval(date1, date2)
    tmp_df[tmp_df$datetime %within% int, "waterET"] <- 0
  }
  
  # Subset columns relevant for randomForest algorithm
  
  #   ### test: model based on 10-min data, don't forget to uncomment subsequent code
  #   tmp_df_sub <- tmp_df[, c("datetime", var_rf)]
  #   tmp_df_sub_agg10m <- aggregate(tmp_df_sub[, 2:ncol(tmp_df_sub)], 
  #                                  by = list(substr(tmp_df_sub[, 1], 1, 15)), 
  #                                  FUN = function(x) mean(x, na.rm = TRUE))
  #   tmp_df_sub_agg10m[, 1] <- paste0(tmp_df_sub_agg10m[, 1], "0")
  #   names(tmp_df_sub_agg10m)[1] <- "datetime"
  #   tmp_df_sub_agg10m$datetime <- strptime(tmp_df_sub_agg10m$datetime, 
  #                                          format = "%Y-%m-%d %H:%M")
  #   
  #   tmp_df_sub <- tmp_df_sub_agg10m[, var_rf]
  #   tmp_df_sub$hour <- hour(tmp_df_sub_agg10m$datetime)
  #   ###
  
  tmp_df_sub <- tmp_df[, var_rf]
  tmp_df_sub <- tmp_df_sub[complete.cases(tmp_df_sub), ]
  
  ## vapor pressure deficit
  tmp_df_sub %>%
    mutate(vpd = vpd(tempUp, humidity)) %>%
    data.frame() -> tmp_df_sub
  
  tmp_ls_rf_stats <- lapply(1:10, function(seed) {

    # data partitioning
    set.seed(seed)
    tmp_int_train <- createDataPartition(tmp_df_sub$waterET, p = .75,
                                         list = FALSE)

    # random forest
    tmp_rf <- train(waterET ~ ., data = tmp_df_sub, method = "rf",
                    subset = tmp_int_train, trControl = fit_control,
                    importance = TRUE)

    # variable importance
    tmp_df_varimp <- varImp(tmp_rf, scale = TRUE)$importance
    tmp_mat_varimp <- t(tmp_df_varimp)
    tmp_df_varimp <- data.frame(tmp_mat_varimp)
    names(tmp_df_varimp) <- colnames(tmp_mat_varimp)

    # optimized training parameters and scores
    int_mtry <- tmp_rf$bestTune
    int_id_mtry <- grep(int_mtry, tmp_rf$results$mtry)

    num_rmse_trn <- tmp_rf$results[int_id_mtry, "RMSE"]
    num_rmse_trn_se <- tmp_rf$results[int_id_mtry, "RMSESD"]
    num_rsq_trn <- tmp_rf$results[int_id_mtry, "Rsquared"]

    # prediction
    tmp_prd <- predict(tmp_rf, newdata = tmp_df_sub[-tmp_int_train, ])

    # prediction parameters
    df_reg_stats <- regressionStats(prd = tmp_prd,
                                    obs = tmp_df_sub[-tmp_int_train, "waterET"],
                                    adj.rsq = FALSE)

    # return output
    list(reg_stats = data.frame(plot = tmp_plt, mtry = int_mtry,
                                rmse_trn = num_rmse_trn, rmse_trns_se = num_rmse_trn_se,
                                rsq_trn = num_rsq_trn, df_reg_stats),
         var_imp = tmp_df_varimp)
  })

  # cv/prediction statistics
  tmp_ls_rf_eval <- lapply(tmp_ls_rf_stats, function(i) i[[1]])
  tmp_df_rf_eval <- do.call("rbind", tmp_ls_rf_eval)
  write.csv(tmp_df_rf_eval, row.names = FALSE, quote = FALSE,
            paste0("data/reprocess/regstats_vpd_", tmp_plt, "_", tmp_dt, ".csv"))
  
  # fls_eval <- paste0("data/reprocess/regstats_vpd_", tmp_plt, "_", tmp_dt, ".csv")
  # tmp_df_rf_eval <- read.csv(fls_eval)
  
  # variable importances
  tmp_ls_rf_varimp <- lapply(tmp_ls_rf_stats, function(i) i[[2]])
  tmp_df_rf_varimp <- do.call("rbind", tmp_ls_rf_varimp)
  tmp_num_rf_varimp <- colMeans(tmp_df_rf_varimp, na.rm = TRUE)
  tmp_mat_rf_varimp <- matrix(tmp_num_rf_varimp, 1, byrow = TRUE)
  tmp_df_rf_varimp <- data.frame(tmp_mat_rf_varimp)
  names(tmp_df_rf_varimp) <- names(tmp_num_rf_varimp)
  write.csv(tmp_df_rf_varimp, row.names = FALSE, quote = FALSE,
            paste0("data/reprocess/varimp_vpd_", tmp_plt, "_", tmp_dt, ".csv"))
  
  # fls_varimp <- paste0("data/reprocess/varimp_vpd_", tmp_plt, "_", tmp_dt, ".csv")
  # tmp_df_rf_varimp <- read.csv(fls_varimp)
  
  # plot prediction stats
  num_reg_stats <- colMeans(tmp_df_rf_eval[, 6:ncol(tmp_df_rf_eval)])
  reg_stats <- num_reg_stats
  p_reg_stats <- plotPredictionStats(reg_stats, digits = 2)
  
  # modal mtry and mean training and prediction scores
  int_mdl_mtry <- modal(tmp_df_rf_eval$mtry)
  num_mu_scores <- colMeans(tmp_df_rf_eval[, 3:ncol(tmp_df_rf_eval)])
  mat_mu_scores <- matrix(num_mu_scores, 1, byrow = TRUE)
  df_mu_scores <- data.frame(mat_mu_scores)
  names(df_mu_scores) <- names(num_mu_scores)
  
  # return data frame and corresponding plot
  list(data.frame(plot = tmp_plt, mtry = int_mdl_mtry, df_mu_scores), 
       data.frame(plot = tmp_plt, mtry = int_mdl_mtry, tmp_df_rf_varimp),
       p_reg_stats)
})

## sort workspaces according to elevation
id <- sapply(slsPlots(style = "elevation"), function(h) {
  grep(h, srunWorkspaces)[1] # rain season only
})

ls_rf_scores_whr1 <- lapply(1:length(srunWorkspaces[id]), function(h) {
  gfPostprocess(srunWorkspaces[id][h], dsn_regstats = "data/regstats_vpd_", 
                dsn_varimp = "data/varimp_vpd_", left = h %in% c(1, 5, 9))
})

ls_rf_scores_whr2 <- lapply(1:length(srunWorkspaces[-id]), function(h) {
  gfPostprocess(srunWorkspaces[-id][h], dsn_regstats = "data/regstats_vpd_", 
                dsn_varimp = "data/varimp_vpd_", left = FALSE, add = TRUE)
})

# split data into cv/prediction statistics...
ls_rf_scores_whr_stats1 <- lapply(ls_rf_scores_whr1, function(i) i[[1]])
ls_rf_scores_whr_stats2 <- lapply(ls_rf_scores_whr2, function(i) i[[1]])
df_rf_scores_stats <- do.call("rbind", append(ls_rf_scores_whr_stats1, 
                                              ls_rf_scores_whr_stats2))
# ...variable importances...
ls_rf_scores_whr_varimp1 <- lapply(ls_rf_scores_whr1, function(i) i[[2]])
ls_rf_scores_whr_varimp2 <- lapply(ls_rf_scores_whr2, function(i) i[[2]])
df_rf_scores_dryssn_varimp <- do.call("rbind", append(ls_rf_scores_whr_varimp1, 
                                                      ls_rf_scores_whr_varimp2))
# ...and referring visualization
ls_rf_scores_dryssn_vis1 <- lapply(ls_rf_scores_whr1, function(i) i[[3]])
ls_rf_scores_dryssn_vis2 <- lapply(ls_rf_scores_whr2, function(i) i[[3]])
ls_rf_scores_dryssn_vis1[[1]] <- ls_rf_scores_dryssn_vis1[[1]] + 
  as.layer(ls_rf_scores_dryssn_vis2[[1]], under = TRUE)
ls_rf_scores_dryssn_vis1[[2]] <- ls_rf_scores_dryssn_vis1[[2]] + 
  as.layer(ls_rf_scores_dryssn_vis2[[2]], under = TRUE)

save(list = c("df_rf_scores_stats", "df_rf_scores_dryssn_varimp", 
              "ls_rf_scores_dryssn_vis1"), file = "data/reg_stats_vpd.RData")

# plotting order
sls_plt <- slsPlots()

fc_rf_plt <- df_rf_scores_stats$plot
rf_plt <- as.character(fc_rf_plt)

int_id_plt <- match(rf_plt, sls_plt)
ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[int_id_plt]

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
  update(ls_rf_scores_dryssn_vis[[i]], main = sls_plt[i])
})

# grid.arrange
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE), 
             ls_rf_scores_dryssn_vis)

# deregister parallel backend
closeAllConnections()
