## environmental stuff

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
ls_pl_ph <- lapply(ch_fls_ws, function(tmp_i) {
  num_pl <- slsPathLength(tmp_i)
  num_ph <- slsPathHeight(tmp_i)

  data.frame(path_length = num_pl, 
             path_height = num_ph)  
})

df_pl_ph <- do.call("rbind", ls_pl_ph)
