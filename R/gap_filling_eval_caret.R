## environmental stuff

# packages
lib <- c("randomForest", "ggplot2", "latticeExtra", "doParallel", "Rsenal", "caret")
sapply(lib, function(x) library(x, character.only = TRUE))

# functions
modal <- function(x) {
  tbl <- table(x)
  ch_mdl <- names(which.max(tbl))
  int_mdl <- as.integer(ch_mdl)
  return(int_mdl)
}

source("R/slsFoggy.R")
source("R/plotPredictionStats.R")

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# path: srun
ch_dir_srun <- "../../SRun/"


## evaluation of random forest performance

srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# training control parameters
fit_control <- trainControl(method = "cv", number = 10, repeats = 10, 
                            verboseIter = TRUE)

ls_rf_scores <- foreach(i = srunWorkspaces, .packages = lib) %dopar% {
  
  tmp_ls_plt <- strsplit(basename(i), "_")
  tmp_ch_plt <- sapply(tmp_ls_plt, "[[", 3)
  
  tmp_fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), 
                        pattern = ".mnd$", full.names = TRUE)
  
  # Merge daily .mnd files
  tmp_df <- slsMergeDailyData(files = tmp_fls)
  
  # fog events (fer, fed and hel only)
  if (tmp_ch_plt %in% c("fer0", "fed1", "hel1")) {
    tmp_df <- slsFoggy(tmp_df, use_error = FALSE, probs = .1)
  }
  
  # Subset columns relevant for randomForest algorithm
  tmp_df_sub <- tmp_df[, c("tempUp", "tempLw", "dwnRad", "upwRad", "humidity",
                           "soilHeatFlux", "pressure", "precipRate", "waterET")]
  tmp_df_sub <- tmp_df_sub[complete.cases(tmp_df_sub), ]
  
  tmp_df_rf_eval <- foreach(seed = 1:10, .combine = "rbind") %do% {

    # data partitioning                          
    set.seed(seed)
    tmp_int_train <- createDataPartition(tmp_df_sub$waterET, p = .75, 
                                         list = FALSE)
    
    # random forest
    tmp_rf <- train(waterET ~ ., data = tmp_df_sub, method = "rf", 
                    subset = tmp_int_train, trControl = fit_control)
    
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
    data.frame(plot = tmp_ch_plt, mtry = mtry, 
               rmse_trn = num_rmse_trn, rmse_trns_se = num_rmse_trn_se, 
               rsq_trn = num_rsq_trn, df_reg_stats)
  }

  #   # plot training stats
  #   num_trn_stats <- colMeans(tmp_df_rf_eval[, 3:5])
  #   names(num_trn_stats) <- c("RMSE", "RMSE.se", "Rsq")
  #   trn_stats <- num_trn_stats
  
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
       p_reg_stats)
}