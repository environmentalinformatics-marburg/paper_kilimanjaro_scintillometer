## environmental stuff

# packages
lib <- c("randomForest", "ggplot2", "latticeExtra", "doParallel")
sapply(lib, function(x) library(x, character.only = TRUE))

# functions
modal <- function(x) {
  tbl <- table(x)
  ch_mdl <- names(which.max(tbl))
  int_mdl <- as.integer(ch_mdl)
  return(int_mdl)
}

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

ls_rf_scores <- lapply(srunWorkspaces, function(i) {
  
  tmp_ls_plt <- strsplit(basename(i), "_")
  tmp_ch_plt <- sapply(tmp_ls_plt, "[[", 3)
  
  tmp_fls <- list.files(paste0(i, "/data/retrieved_SPU-111-230"), 
                        pattern = ".mnd$", full.names = TRUE)
  
  # Merge daily .mnd files
  tmp_df <- slsMergeDailyData(files = tmp_fls)
  
  # Subset columns relevant for randomForest algorithm
  tmp_df_sub <- tmp_df[, c("tempUp", "tempLw", "dwnRad", "upwRad", "humidity",
                           "soilHeatFlux", "pressure", "precipRate", "waterET")]
  tmp_df_sub <- tmp_df_sub[complete.cases(tmp_df_sub), ]
  
  tmp_df_rf_eval <- foreach(seed = 1:10, .packages = lib, 
                            .combine = "rbind") %dopar% {

    # data partitioning                          
    set.seed(seed)
    tmp_int_train <- createDataPartition(tmp_df_sub$waterET, p = .75, 
                                         list = FALSE)
    
    # random forest
    tmp_rf <- train(waterET ~ ., data = tmp_df_sub, method = "rf", 
                    subset = tmp_int_train, trControl = fit_control)
    
    # optimized training parameters and scores
    int_mtry <- tmp_rf$bestTune
    num_rmse_trn <- tmp_rf$results[grep(int_mtry, tmp_rf$results$mtry), "RMSE"]
    num_rsq_trn <- tmp_rf$results[grep(int_mtry, tmp_rf$results$mtry), "Rsquared"]
    
    # prediction
    tmp_prd <- predict(tmp_rf, newdata = tmp_df_sub[-tmp_int_train, ])
    
    # prediction parameters
    num_rmse_tst <- with(tmp_df_sub[-tmp_int_train, ], 
                         sqrt(mean((waterET-tmp_prd)^2)))
    
    tmp_lm <- lm(tmp_prd ~ tmp_df_sub[-tmp_int_train, "waterET"])
    num_rsq <- summary(tmp_lm)$r.squared
    num_p <- summary(tmp_lm)$coefficients[2, 4]
    
    # return output
    data.frame(plot = tmp_ch_plt, mtry = mtry, 
               rmse_trn = num_rmse_trn, rsq_trn = num_rsq_trn,
               rmse_tst = num_rmse_tst, rsq = num_rsq, p = num_p)
  }

  int_mdl_mtry <- modal(tmp_df_rf_eval$mtry)
  num_mu_scores <- colMeans(tmp_df_rf_eval[, 3:7])
  mat_mu_scores <- matrix(num_mu_scores, 1, 5)
  df_mu_scores <- data.frame(mat_mu_scores)
  names(df_mu_scores) <- names(num_mu_scores)
  
  data.frame(plot = tmp_ch_plt, mtry = int_mdl_mtry, df_mu_scores)
})