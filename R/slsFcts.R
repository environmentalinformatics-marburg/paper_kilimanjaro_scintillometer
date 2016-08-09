## r functions
fun <- paste0("R/", c("slsPlots", "slsAvlFls", "slsDiurnalVariation", 
                 "slsAggregate", "slsMergeDailyData", "slsFoggy", 
                 "plotPredictionStats", "mergeGPX", "panel.smoothconts", 
                 "vpd", "qcMyd17", "qcxMyd15", "slsTemporalRange", 
                 "slsGppIndex", "madjacent", "summarizeVar", 
                 "gfPostprocess", "visKili", "slsPathLength", 
                 "slsPathSlope", "slsPathHeight", "slsPathInclination"), ".R")
jnk <- sapply(fun, source)

## c++ functions
library(Rcpp)
sourceCpp("src/SlsCppFun.cpp")

## strip labeller (required in visMetData)
## (found on http://stackoverflow.com/questions/11979017/changing-facet-label-to-math-formula-in-ggplot2)
facet_wrap_labeller <- function(gg.plot,labels=NULL) {
  #works with R 3.0.1 and ggplot2 0.9.3.1
  require(gridExtra)
  
  g <- ggplotGrob(gg.plot)
  gg <- g$grobs      
  strips <- grep("strip_t", names(gg))
  
  for(ii in seq_along(labels))  {
    modgrob <- getGrob(gg[[strips[ii]]], "strip.text", 
                       grep=TRUE, global=TRUE)
    gg[[strips[ii]]]$children[[modgrob$name]] <- editGrob(modgrob,label=labels[ii])
  }
  
  print.arrange <- function(x){
    grid::grid.draw(x)
  }
  
  g$grobs <- gg
  class(g) = c("arrange", "ggplot",class(g)) 
  g
}