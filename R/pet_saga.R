library(RSAGA)

# set up rsaga environment:
work_env <- rsaga.env() 
work_env 

# retrieve list of available libraries
ch_saga_lib <- rsaga.get.libraries()

sapply(ch_saga_lib, rsaga.get.modules)
rsaga.get.modules("grid_filter")
