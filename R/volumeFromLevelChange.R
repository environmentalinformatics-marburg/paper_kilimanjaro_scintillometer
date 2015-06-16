volumeFromLevelChange <- function(change, d = 2.97, to_mm = TRUE) {
  
  ## circular area
  num_area <- (d/2)^2 * pi
  
  ## change in liters
  num_ltr <- num_area * change

  if (to_mm) {
    
    ## 1 mm (0.01 dm) equivalent in liters
    num_ltr_mm <- 0.01 * num_area
    
    ## change in millimeters
    return(num_ltr / num_ltr_mm)
    
  } else {
    
    return(num_ltr)
  }
}
