lib <- c("doParallel", "caret", "lubridate", "plotrix", "plyr", "dplyr", 
         "reshape2", "scales", "ggplot2", "latticeExtra", "gridExtra", 
         "TSdist", "Rsenal", "OpenStreetMap", "Orcs", "rgeos")

jnk <- sapply(lib, function(i) {
  suppressMessages(library(i, character.only = TRUE))
})
