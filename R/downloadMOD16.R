downloadMOD16 <- function(begin, end, interval = "8-day", format = "%Y-%m-%d") {
  
  ## transform 'begin' and 'end' to 'Date' object if necessary
  if (!is.Date(begin))
    begin <- as.Date(begin, format = format)
  
  if (!is.Date(end))
    end <- as.Date(end, format = format)
  
  ## mod16 ftp server
  ch_url <- "ftp://ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/"
  
  if (interval == "8-day") {
    ch_url <- paste0(ch_url, "MOD16A2.105_MERRAGMAO/")
    downloadMOD16A2(begin = begin, end = end, ftp = ch_url)
  } else if (interval == "monthly") {
    ch_url <- paste0(ch_url, "MOD16A2_MONTHLY.MERRA_GMAO_1kmALB/")
    downloadMOD16A2(begin = begin, end = end, ftp = ch_url)
  } else if (interval == "annual") {
    ch_url <- paste0(ch_url, "MOD16A3.105_MERRAGMAO/")
    downloadMOD16A3(begin = begin, end = end, ftp = ch_url)
  } else {
    stop("Please supply a valid interval (8-day, monthly or annual).")
  }
  
}