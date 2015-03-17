## environmental stuff

# packages
lib <- c("randomForest", "ggplot2", "latticeExtra", "doParallel")
sapply(lib, function(x) library(x, character.only = TRUE))

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# functions
source("phd/scintillometer/src/slsMergeDailyData.R")

# path: srun
ch_dir_srun <- "../../SRun/"

srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

lapply(srunWorkspaces, function(i) {
  
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
  
  tmp_df_rf_eval <- foreach(seed = 1:100, .packages = lib, 
                            .combine = "rbind") %dopar% {
                              
    set.seed(seed)
    tmp_int_train <- sample(1:nrow(tmp_df_sub), 1500)
    
    # oob and test error (cf. statistical learning course, chapter 8)
    num_err_oob <- double(8)
    num_err_tst <- double(8)
    
    for (mtry in 1:8) {
      # random forest
      tmp_rf <- randomForest(waterET ~ ., data = tmp_df_sub, 
                             subset = tmp_int_train, ntree = 400, mtry = mtry)
      
      # obb error
      num_err_oob[mtry] <- tmp_rf$mse[length(tmp_rf$mse)]
      
      # prediction and test error
      tmp_prd <- predict(tmp_rf, newdata = tmp_df_sub[-tmp_int_train, ])
      
      num_err_tst[mtry] <- with(tmp_df_sub[-tmp_int_train, ], 
                                mean((waterET-tmp_prd)^2))
    }
    
    mat_err <- cbind(num_err_oob, num_err_tst)
    matplot(1:mtry, mat_err, pch = 19, col = c("blue", "red"), type = "b", 
            xlab = expression(italic(mtry)), ylab = "Mean squared error")
    legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("blue","red"))
    
    # selection of mtry based on fixed threshold (change?)
    mtry <- (1:8)[which(abs(diff(num_err_tst)) < .001)[1]]
    
    #   # visualization of obb and test error subject to ntree
    #   mtry <- 3
    #   for (ntree in 1:10) {
    #     # random forest
    #     tmp_rf <- randomForest(waterET ~ ., data = tmp_df_sub, 
    #                            subset = tmp_int_train, ntree = ntree * 50, mtry = mtry)
    #     
    #     # obb error
    #     num_err_oob[ntree] <- tmp_rf$mse[length(tmp_rf$mse)]
    #     
    #     # prediction and test error
    #     tmp_prd <- predict(tmp_rf, newdata = tmp_df_sub[-tmp_int_train, ])
    #     
    #     num_err_tst[ntree] <- with(tmp_df_sub[-tmp_int_train, ], 
    #                                mean((waterET-tmp_prd)^2))
    #   }
    # 
    #   mat_err <- cbind(num_err_oob, num_err_tst)
    #   matplot((1:ntree)*50, mat_err, pch = 19, col = c("blue", "red"), type = "b", 
    #           xlab = expression(italic(ntree)), ylab = "Mean squared error")
    #   legend("topright", legend = c("OOB", "Test"), pch = 19, col = c("blue","red"))
    
    valid <- FALSE
    niter <- 1
    while (!valid) {
      set.seed(niter)
      index <- sample(1:nrow(tmp_df_sub), 1000)
      
      train <- tmp_df_sub[-index, ]
      
      if (all(levels(train$waterET) %in% 
                unique(levels(train$waterET)[train$waterET]))) {
        valid <- TRUE
      } else {
        niter <- niter + 1
      }
    }
    
    # random forest
    rf <- randomForest(waterET ~ ., data = tmp_df_sub, subset = tmp_int_train, 
                       ntree = 400, mtry = mtry)
    
    # prediction
    prd <- predict(rf, newdata = tmp_df_sub[-tmp_int_train, ])
    
    # r-squared
    mod <- lm(prd ~ tmp_df_sub[-tmp_int_train, "waterET"])
    num_rsq <- summary(mod)$r.squared
    num_p <- summary(mod)$coefficients[2, 4]
    
    #   if (!file.exists(paste0(i, "/data/eval")))
    #     dir.create(paste0(i, "/data/eval"))
    #   
    #   png(paste0(i, "/data/eval/", plt, "_rf_eval.png"), width = 20, height = 20, 
    #       units = "cm", pointsize = 16, res = 300)
    #   print(xyplot(prd ~ tmp_df_sub[-tmp_int_train, "waterET"], 
    #                main = tmp_ch_plt, col = "grey75", xlab = "Measured ET [mm/h]", 
    #                ylab = "Predicted ET [mm/h]") + 
    #           layer(panel.ablineq(lm(y ~ x), r.squared = TRUE, rotate = TRUE, at = .8, 
    #                               pos = 1)))
    #   dev.off()
    
    data.frame(plot = tmp_ch_plt, mtry = mtry, err_oob = num_err_oob[mtry], 
               err_tst = num_err_tst[mtry], rsq = num_rsq, p = num_p)
  }
})