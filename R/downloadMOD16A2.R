downloadMOD16A2 <- function(begin, end, interval = "8-day", ftp = NULL) {
  
  ## mod16 ftp server
  if (!is.null(ftp)) {
    ch_url <- ftp
  } else {
    
    ch_url <- "ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/"
    
    if (interval == "8-day") {
      ch_url <- paste0(ch_url, "MOD16A2.105_MERRAGMAO/")
    } else if (interval == "monthly") {
      ch_url <- paste0(ch_url, "MOD16A2_MONTHLY.MERRA_GMAO_1kmALB/")
    } else {
      stop("Please supply a valid interval (8-day, monthly or annual).")
    }
  }
  
}