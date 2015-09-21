### environmental stuff

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# path: srun
ch_dir_srun <- "../../SRun/"


### data processing

srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# gap-filling and 10m/1h aggregation
ls_sls_valid <- lapply(srunWorkspaces, function(i) {
  
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
  
  ## hourly aggregation
  tmp_df_agg <-slsAggregate(tmp_df, agg_by = 60, 
                            FUN = function(...) mean(..., na.rm = TRUE))
  
  int_isvalid <- which(!is.na(tmp_df_agg$waterET))
  tmp_df_agg <- tmp_df_agg[int_isvalid[1]:int_isvalid[length(int_isvalid)], ]
  data.frame(plotid = tmp_ch_plt, 
             valid = sum(!is.na(tmp_df_agg$waterET)), 
             invalid = sum(is.na(tmp_df_agg$waterET)), 
             missing = sum(is.na(tmp_df_agg$waterET)) / nrow(tmp_df_agg))
})
df_sls_valid <- do.call("rbind", ls_sls_valid)  

## share of missing hourly et rates
sum(df_sls_valid[, 3]) /
  sum(colSums(df_sls_valid[, c(2, 3)]))
