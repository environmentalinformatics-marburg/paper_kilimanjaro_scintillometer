lib <- c("doParallel", "caret", "lubridate", "plotrix", "plyr", "dplyr", 
         "reshape2", "scales", "ggplot2", "latticeExtra", "gridExtra")
jnk <- sapply(lib, function(i) library(i, character.only = TRUE, 
                                       quietly = TRUE, verbose = FALSE))
