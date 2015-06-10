### environmental stuff

## workspace clearance
rm(list = ls(all = TRUE))

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

modal <- function(x) {
  tbl <- table(x)
  ch_mdl <- names(which.max(tbl))
  int_mdl <- as.integer(ch_mdl)
  return(int_mdl)
}

## parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

## path: srun
ch_dir_srun <- "../../SRun/"


### data processing

srunWorkspaces <- dir(ch_dir_srun, pattern = "^workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# training control parameters
fit_control <- trainControl(method = "cv", number = 10, repeats = 10, 
                            verboseIter = TRUE)

# variables relevant for rf procedure
ch_var_rf <- c("tempUp", "tempLw", "dwnRad", "upwRad", "humidity",
               "soilHeatFlux", "pressure", "waterET")

ls_rf_scores_whr <- lapply(srunWorkspaces, function(i) {
  
  tmp_ls_plt <- strsplit(basename(i), "_")
  tmp_ch_plt <- sapply(tmp_ls_plt, "[[", 3)
  tmp_ch_dt <- sapply(tmp_ls_plt, "[[", 4)
  
  tmp_fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), 
                        pattern = ".mnd$", full.names = TRUE)
  
  ## merge daily files
  tmp_df <- slsMergeDailyData(files = tmp_fls)
  tmp_df$datetime <- strptime(tmp_df$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  ## adjust et rates < 0
  num_et <- tmp_df[, "waterET"]
  log_st0 <- which(!is.na(num_et) & num_et < 0)
  if (length(log_st0) > 0)
    tmp_df[log_st0, "waterET"] <- 0
  
  # fog events (fer, fed and hel only)
  if (tmp_ch_plt %in% c("fer0", "fed1", "hel1")) {
    tmp_df_fog <- slsFoggy(tmp_df, use_error = FALSE, probs = .1)
    tmp_df[tmp_df_fog$fog, ch_var_rf] <- NA
  }

  ## fill mai4 nighttime et
  if (tmp_ch_plt == "mai4") {
    date1 <- as.POSIXct("2014-05-13 22:00:00")
    date2 <- as.POSIXct("2014-05-14 05:59:00")
    int <- new_interval(date1, date2)
    tmp_df[tmp_df$datetime %within% int, "waterET"] <- 0
    
    date1 <- as.POSIXct("2014-05-15 22:00:00")
    date2 <- as.POSIXct("2014-05-16 05:59:00")
    int <- new_interval(date1, date2)
    tmp_df[tmp_df$datetime %within% int, "waterET"] <- 0
  }
  
  # Subset columns relevant for randomForest algorithm
  tmp_df_sub <- tmp_df[, ch_var_rf]
  tmp_df_sub$hour <- hour(tmp_df$datetime)
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
    list(reg_stats = data.frame(plot = tmp_ch_plt, mtry = int_mtry, 
                                rmse_trn = num_rmse_trn, rmse_trns_se = num_rmse_trn_se, 
                                rsq_trn = num_rsq_trn, df_reg_stats), 
         var_imp = tmp_df_varimp)
  })

  # cv/prediction statistics
  tmp_ls_rf_eval <- lapply(tmp_ls_rf_stats, function(i) i[[1]])
  tmp_df_rf_eval <- do.call("rbind", tmp_ls_rf_eval)
  write.csv(tmp_df_rf_eval, row.names = FALSE, quote = FALSE, 
            paste0("data/regstats_", tmp_ch_plt, "_", tmp_ch_dt, ".csv"))
  
  # variable importances
  tmp_ls_rf_varimp <- lapply(tmp_ls_rf_stats, function(i) i[[2]])
  tmp_df_rf_varimp <- do.call("rbind", tmp_ls_rf_varimp)
  tmp_num_rf_varimp <- colMeans(tmp_df_rf_varimp, na.rm = TRUE)
  tmp_mat_rf_varimp <- matrix(tmp_num_rf_varimp, 1, byrow = TRUE)
  tmp_df_rf_varimp <- data.frame(tmp_mat_rf_varimp)
  names(tmp_df_rf_varimp) <- names(tmp_num_rf_varimp)
  write.csv(tmp_df_rf_varimp, row.names = FALSE, quote = FALSE,
            paste0("data/varimp_", tmp_ch_plt, "_", tmp_ch_dt, ".csv"))
  
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
  list(data.frame(plot = tmp_ch_plt, mtry = int_mdl_mtry, df_mu_scores), 
       data.frame(plot = tmp_ch_plt, mtry = int_mdl_mtry, tmp_df_rf_varimp),
       p_reg_stats)
}

source("R/gap_filling_postprocess.R")

# wet season data only
int_id_dryssn <- c(11, 13)
ls_rf_scores_dryssn <- ls_rf_scores_whr[-int_id_dryssn]

# split data into cv/prediction statistics...
ls_rf_scores_dryssn_stats <- lapply(ls_rf_scores_dryssn, function(i) i[[1]])
df_rf_scores_stats <- do.call("rbind", ls_rf_scores_dryssn_stats)
# ...variable importances...
ls_rf_scores_dryssn_varimp <- lapply(ls_rf_scores_dryssn, function(i) i[[2]])
df_rf_scores_dryssn_varimp <- do.call("rbind", ls_rf_scores_dryssn_varimp)
# ...and referring visualization
ls_rf_scores_dryssn_vis <- lapply(ls_rf_scores_dryssn, function(i) i[[3]])

# save(list = c("df_rf_scores_stats", "df_rf_scores_dryssn_varimp", 
#               "ls_rf_scores_dryssn_vis"), file = "data/reg_stats_whr.RData")

# plotting order
ch_sls_plt <- slsPlots()

fc_rf_plt <- df_rf_scores_stats$plot
ch_rf_plt <- as.character(fc_rf_plt)

int_id_plt <- match(ch_rf_plt, ch_sls_plt)
ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[int_id_plt]

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
  update(ls_rf_scores_dryssn_vis[[i]], main = ch_sls_plt[i])
})

# grid.arrange
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE), 
             ls_rf_scores_dryssn_vis)

# deregister parallel backend
closeAllConnections()
