lib <- c("doParallel", "caret", "lubridate", "plotrix", "dplyr", 
         "ggplot2", "latticeExtra", "gridExtra")
jnk <- sapply(lib, function(i) library(i, character.only = TRUE))
