fun <- c("R/slsPlots.R", "R/slsAvlFls.R", "R/slsDiurnalVariation.R", 
         "R/slsAggregate.R", "R/slsMergeDailyData.R", "R/slsFoggy.R", 
         "R/plotPredictionStats.R", "R/mergeGPX.R", "R/panel.smoothconts.R", 
         "R/vpd.R")
jnk <- sapply(fun, source)
