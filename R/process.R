## Environmental stuff

# packages
lib <- c("randomForest", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# functions
source("R/slsMergeDailyData.R")
source("R/slsProcessing.R")

# srun path
ch_dir_srun <- "../../SRun/"


## data

srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# gap-filling and 1h aggregation
ls_sls <- lapply(srunWorkspaces, function(i) {
  slsProcessing(i, dsn = "data/sls")
})

ch_sls_1h <- list.files("../../phd/scintillometer/data", full.names = TRUE,
                        pattern = "_mrg_rf_agg01h.csv", recursive = TRUE)

ls_sls_1h <- lapply(ch_sls_1h, function(i) {
  tmp_ch_fls_sh <- basename(i)
  tmp_ls_fls_sh <- strsplit(tmp_ch_fls_sh, "_")
  tmp_ch_plt <- sapply(tmp_ls_fls_sh, "[[", 1)
  tmp_df <- read.csv(i)
  return(data.frame(plotid = tmp_ch_plt, tmp_df))
})
df_sls_1h <- do.call("rbind", ls_sls_1h)
df_sls_1h$datetime <- strptime(df_sls_1h$datetime, format = "%Y-%m-%d %H:%M")
