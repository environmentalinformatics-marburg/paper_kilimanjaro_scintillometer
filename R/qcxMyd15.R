qcxMyd15 <- function(x, len = 8, snow = TRUE, cirrus = TRUE, 
                    clouds = TRUE, shadow = TRUE, ...) {
  
  stopifnot(require(Rsenal))
  
  int_bin <- number2binary(x, len)
  int_bin <- rev(int_bin)
  
  # Snow/ice detected
  log_snow <- int_bin[3] == 1
  # Cirrus detected
  log_cirrus <- int_bin[5] == 1
  # Internal cloud mask
  log_clouds <- int_bin[6] == 1
  # Cloud shadow
  log_shadow <- int_bin[7] == 1
  
  if (snow)
    if (log_snow) x <- NA
  
  if (cirrus)
    if (log_cirrus) x <- NA
  
  if (clouds)
    if (log_clouds) x <- NA
  
  if (shadow)
    if (log_shadow) x <- NA
  
  return(x)
}
