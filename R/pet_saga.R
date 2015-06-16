################################################################################
### code taken from http://www.r-bloggers.com/rsaga-getting-started/         ###
################################################################################

library(RSAGA)

## set up 'rsaga' environment (dependent upon OS)
?rsaga.env

if (Sys.info()[["sysname"]] == "Windows") {
  work_env <- rsaga.env(path = "C:/Program Files (x86)/SAGA-GIS") 
} else {
  work_env <- rsaga.env()
}

## list of available libraries
ch_saga_lib <- rsaga.get.libraries(path = work_env$modules)

## modules in library 'climate_tools'
?rsaga.get.modules
ls_modules <- rsaga.get.modules("climate_tools", env = work_env)

## usage of 'pet' module
?rsaga.get.usage
rsaga.get.usage("climate_tools", module = 6, env = work_env)

## convert required grid files to saga grid format
# ?rsaga.import.gdal
# rsaga.import.gdal('lidar.tif')