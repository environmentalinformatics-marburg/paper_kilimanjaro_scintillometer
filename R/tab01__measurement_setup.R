## environmental stuff

# packages
library(foreach)

# functions
source("R/slsPathLength.R")
source("R/slsPathHeight.R")

# sls workspace path and files (sub-directories)
ch_dir_ws <- "/media/permanent/SRun"
ch_fls_ws <- list.files(ch_dir_ws, pattern = "^workspace_SLS", 
                        full.names = TRUE)


## data

# sampled research sites
ls_plt <- strsplit(ch_fls_ws, "_")
ch_plt <- sapply(ls_plt, "[[", 3)

# extraction of relevant information
df_cfg <- foreach(tmp_fls_ws = ch_fls_ws, tmp_plt = ch_plt, 
                  .combine = "rbind") %do% {
  num_pl <- slsPathLength(tmp_fls_ws)
  num_ph <- slsPathHeight(tmp_fls_ws)
  
  data.frame(plot = tmp_plt, 
             path_length = num_pl, 
             path_height = num_ph)  
}

# shortest/longest path length/height
df_cfg[which.min(df_cfg$path_length), ]
df_cfg[which.max(df_cfg$path_length), ]

df_cfg[which.min(df_cfg$path_height), ]
df_cfg[which.max(df_cfg$path_height), ]
