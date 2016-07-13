lib <- c("doParallel", "caret", "lubridate", "plotrix", "plyr", "dplyr", 
         "reshape2", "scales", "ggplot2", "latticeExtra", "gridExtra", "grid",
         "TSdist", "Rsenal", "Orcs", "rgeos", "rasterVis", 
         "rgdal", "MODIS", "zoo", "raster", "kza")

jnk <- sapply(lib, function(i) {
  suppressMessages(library(i, character.only = TRUE))
})

# library(OpenStreetMap)