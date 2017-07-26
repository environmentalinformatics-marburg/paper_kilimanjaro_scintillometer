## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

# output path
ch_dir_os <- switch(Sys.info()[["sysname"]], 
                    "Linux" = "/media/permanent/", 
                    "Windows" = "C:/Permanent/")

ch_dir_ppr <- paste0(ch_dir_os, 
                     "publications/paper/detsch_et_al__spotty_evapotranspiration/")

# sls plots and referring files
ch_sls_plt <- slsPlots()
df_sls_fls_rs <- slsAvlFls("/media/permanent/phd/scintillometer/data/sls/reprocess", ssn = "r")

## hourly aggregation
ls_all <- lapply(df_sls_fls_rs[, "mrg_rf_agg01h"], function(i) {
  tmp_ch_plot <- substr(basename(i), 1, 4)
  
  ## import, reformat and merge daily data
  tmp_df <- read.csv(i, stringsAsFactors = FALSE)
  tmp_df <- tmp_df[, c("datetime", "waterET")]
  tmp_df[, "Time"] <- sapply(strsplit(tmp_df[, "datetime"], " "), "[[", 2)
  tmp_ls <- split(tmp_df, yday(tmp_df$datetime))
  tmp_df_mrg <- Reduce(function(...) merge(..., by = "Time"), tmp_ls)
  tmp_df_mrg <- tmp_df_mrg[, -grep("datetime", names(tmp_df_mrg))]
  
  ## hourly means (mu) and standard errors (se)
  tmp_num_mu <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) mean(..., na.rm = TRUE))
  tmp_num_se <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) std.error(..., na.rm = TRUE))
  
  ## merge and return relevant data
  tmp_df_all <- data.frame(PlotID = tmp_ch_plot, Time = tmp_df_mrg[, "Time"], 
                           ETmu = tmp_num_mu, ETse = tmp_num_se, 
                           stringsAsFactors = FALSE)
  return(tmp_df_all)
})
df_all <- do.call("rbind", ls_all)

## add 'habitat' column
df_all$habitat <- substr(df_all$PlotID, 1, 3)
ch_habitats <- unique(df_all$habitat)

## calculate r-squared, p and euclidean distance
ls_sls_scores <- lapply(ch_habitats, function(i) {
  tmp_df <- subset(df_all, habitat == i)
  tmp_ls <- split(tmp_df, tmp_df$PlotID)
  
  if (length(tmp_ls) > 1) {
    tmp_x <- tmp_ls[[1]]$ETmu
    tmp_y <- tmp_ls[[2]]$ETmu

    # r-squared
    tmp_lm <- lm(tmp_y ~ tmp_x)
    tmp_rsq <- summary(tmp_lm)$r.squared
    tmp_p <- summary(tmp_lm)$coefficients[2, 4]
    
    # dissimilarity
    log_x_isna <- is.na(tmp_x)
    log_y_isna <- is.na(tmp_y)
    log_isna <- log_x_isna | log_y_isna
    if (any(log_isna)) {
      tmp_x_nona <- tmp_x[-which(log_isna)]
      tmp_y_nona <- tmp_y[-which(log_isna)]
    } else {
      tmp_x_nona <- tmp_x
      tmp_y_nona <- tmp_y
    }
    tmp_edist <- euclideanDistance(tmp_x_nona, tmp_y_nona)
    
    tmp_df_scores <- data.frame(habitat = i, 
                                rsq = tmp_rsq, p = tmp_p, edist = tmp_edist)
  } else {
    tmp_df_scores <- data.frame(habitat = i, 
                                rsq = NA, p = NA, edist = NA)
  }
  
  return(tmp_df_scores)
})
df_sls_scores <- do.call("rbind", ls_sls_scores)
