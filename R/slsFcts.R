## r functions
fun <- c("R/slsPlots.R", "R/slsAvlFls.R", "R/slsDiurnalVariation.R", 
         "R/slsAggregate.R", "R/slsMergeDailyData.R", "R/slsFoggy.R", 
         "R/plotPredictionStats.R", "R/mergeGPX.R", "R/panel.smoothconts.R", 
         "R/vpd.R", "R/qcMyd17.R", "R/qcxMyd15.R", "R/slsTemporalRange.R", 
         "R/slsGppIndex.R", "R/madjacent.R", "R/summarizeVar.R", 
         "R/gfPostprocess.R", "R/visKili.R")
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