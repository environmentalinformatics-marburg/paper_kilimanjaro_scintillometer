## r functions
fun <- c("R/slsPlots.R", "R/slsAvlFls.R", "R/slsDiurnalVariation.R", 
         "R/slsAggregate.R", "R/slsMergeDailyData.R", "R/slsFoggy.R", 
         "R/plotPredictionStats.R", "R/mergeGPX.R", "R/panel.smoothconts.R", 
         "R/vpd.R", "R/qcMyd17.R", "R/qcxMyd15.R", "R/slsTemporalRange.R", 
         "R/slsGppIndex.R")
jnk <- sapply(fun, source)

## c++ functions
library("Rcpp")
sourceCpp("src/SlsCppFun.cpp")
