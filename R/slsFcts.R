fun <- c("R/slsPlots.R", "R/slsAvlFls.R", "R/slsDiurnalVariation.R", 
         "R/slsAggregate.R", "R/slsMergeDailyData.R", "R/slsFoggy.R", 
         "R/plotPredictionStats.R")
jnk <- sapply(fun, source)
