slsPlots <- function(style = NULL) {
  
  if (!is.null(style)) {
    if (style == "ggplot") {
      return(rev(c("sav5", "sav0", "mai4", "mai0", 
                   "gra2", "gra1", "cof2", "cof3", 
                   "fed1", "fer0", "", "hel1")))
    } else if (style == "elevation") {
      return(c("sav5", "sav0", "mai4", "mai0", 
               "cof3", "cof2", "gra1", "gra2", 
               "fed1", "hel1", "fer0"))
    }
  } else {
    return(c("sav0", "sav5", "mai0", "mai4", 
             "gra1", "gra2", "cof3", "cof2", 
             "fer0", "fed1", "hel1"))
  }
}