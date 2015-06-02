lib <- c("doParallel", "caret", "lubridate", "plotrix", "plyr", "dplyr", 
         "reshape2", "scales", "ggplot2", "latticeExtra", "gridExtra", 
         "TSdist", "Rsenal")

jnk <- sapply(lib, function(i) {
  suppressMessages(library(i, character.only = TRUE))
})
