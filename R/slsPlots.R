slsPlots <- function(style = NULL, empty = 2L, nc = 2L, dry = FALSE) {
  
  if (!is.null(style)) {
    if (style == "ggplot") {
      
      if (nc == 4L)
        plots <- rev(c("mai0", "mai4", "sav0", "sav5", 
                       "gra2", "gra1", "cof2", "cof3", 
                       "fer0", "hel1", "fed1"))
      else if (nc == 2L)
        plots <- rev(c("sav0", "sav5", "mai0", "mai4", 
                       "cof2", "cof3", "gra2", "gra1", 
                       "hel1", "fed1", "fer0"))
      else
        stop("Number of columns not supported yet. Please add to function. \n")
      
      if (empty == 1)
        plots <- c("", plots)
      else 
        plots <- c(plots[1:(empty-1)], "", plots[(empty):length(plots)])
      
      return(plots)
    } else if (style == "elevation") {
      plots <- c("sav5", "sav0", "mai4", "mai0", 
                 "cof3", "cof2", "gra1", "gra2", 
                 "fed1", "hel1", "fer0")
      
      if (dry) {
        plots <- c(plots[1], paste(plots[1], "(d)"), 
                   plots[2], paste(plots[2], "(d)"), 
                   plots[3:length(plots)])
      }
      
      return(plots)
    }
  } else {
    return(c("sav0", "sav5", "mai0", "mai4", 
             "gra1", "gra2", "cof3", "cof2", 
             "fer0", "fed1", "hel1"))
  }
}