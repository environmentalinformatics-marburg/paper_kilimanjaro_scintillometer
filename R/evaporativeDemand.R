evaporativeDemand <- function(change, d = 2.97) {
  
  ## circular area
  num_area <- (d/2)^2 * pi
  
  ## change in liters
  num_ltr <- num_area * change
  
  ## change in mm
  num_mm <- num_ltr / num_area
  
  return(num_mm)
}