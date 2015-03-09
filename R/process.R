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

ls_sls <- lapply(srunWorkspaces, function(i) {
  slsProcessing(i, dsn = "data/sls")
})
