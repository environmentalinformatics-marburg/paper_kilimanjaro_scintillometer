## packages
library(foreach)

## config files
source("R/slsAvlFls.R")
cfg <- slsAvlFls("../../SRun", ".conf$")
cfg <- cfg[, ncol(cfg)]
cfg <- cfg[grep("retrieved", cfg)]

## relevant config parameters and reference values
prm <- list("model"
            , "calibTransmitterWavelength", "calibTransmitterSeparationXY"
            , "calibReceiverSeparationXAYA", "calibReceiverSeparationXBYB")

ref <- list("SLS40", 676, 2.54, 2.57, 2.6)

## loop over config files and, if required, update config parameters 
for (i in cfg) {
  lns <- readLines(i)

  updated <- foreach(j = prm, k = ref, .combine = "c") %do% {
    ids <- grep(paste0("^", j), lns)
    if (length(ids) > 1) stop("More than one line identified.\n")
    
    val <- getSLSConfig(lns, param = j)
    if (!identical(val, k)) {
      lns[ids] <- gsub(val, k, lns[ids])
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  # if anything has been modified, write newly created contents to disk
  if (any(updated))
    writeLines(lns, i)
}


### retrieve parameter from configuration file -----

getSLSConfig <- function(x, param = "model") {
  ids <- grep(paste0("^", param), x)
  if (length(ids) > 1) stop("More than one line identified.\n")
  
  out <- sapply(strsplit(x[ids], "="), "[[", 2)
  if (param != "model") out <- as.numeric(out)
  
  return(out)
}
