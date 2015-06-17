downloadMOD16A2 <- function(begin, end, interval = "8-day", tileH, tileV, 
                            ftp = NULL, ...) {
  
  ## required packages
  stopifnot(require(RCurl))
 
  ## target tile
  tileH <- formatC(tileH, width = 2, flag = 0)
  tileV <- formatC(tileV, width = 2, flag = 0)
  ch_tls_pttrn <- paste0("h", tileH, "v", tileV)
  
  ## target directory
  ch_dir_arcp <- getOption("MODIS_localArcPath")
  ch_dir_out <- paste(ch_dir_arcp, "MODIS", "MOD16A2.105/", sep = "/")
  
  if (!file.exists(ch_dir_out))
    dir.create(ch_dir_out)
  
  ## mod16 ftp server
  if (!is.null(ftp)) {
    ch_url <- ftp
  } else {
    
    ch_url <- "ftp://anonymous@ftp.ntsg.umt.edu/pub/MODIS/NTSG_Products/MOD16/"
    
    if (interval == "8-day") {
      ch_url <- paste0(ch_url, "MOD16A2.105_MERRAGMAO/")
    } else if (interval == "monthly") {
      ch_url <- paste0(ch_url, "MOD16A2_MONTHLY.MERRA_GMAO_1kmALB/")
    } else {
      stop("Please supply a valid interval (8-day, monthly or annual).")
    }
  }
  
  ## available years
  con_url <- url(ch_url)
  ch_yrs <- getURL(con_url, verbose = FALSE, dirlistonly = TRUE, 
                   userpwd = "anonymous:")
  flush(ch_url)
  ls_yrs <- strsplit(ch_yrs, "\n")
  ch_yrs <- unlist(ls_yrs)
  
  ## loop over years
  int_yrs_seq <- seq(year(begin), year(end), 1)
  
  for (i in int_yrs_seq) {
    
    int_id_yr <- grep(i, ch_yrs)
    ch_url_yr <- paste0(ch_url, ch_yrs[int_id_yr], "/")
    
    ## available 8-day records per year
    ch_dys <- getURL(ch_url_yr, verbose = FALSE, dirlistonly = TRUE, 
                     userpwd = "anonymous:")
    ls_dys <- strsplit(ch_dys, "\n")
    ch_dys <- unlist(ls_dys)
    
    ## determine starting point in current year
    ch_dys_sub <- gsub("D", "", ch_dys)
    int_dys <- sort(as.integer(ch_dys_sub))
    
    if (year(begin) == i) {
      int_dys_diff <- int_dys - yday(begin)
      int_id_start <- which(int_dys_diff <= 0)[1]
    } else {
      int_id_start <- 1
    }
    int_dy_start <- int_dys[int_id_start]
    
    ## determine end point in current year
    if (year(end) == i) {
      int_dys_diff <- int_dys - yday(end)
      int_id_end <- which(int_dys_diff <= 0)[length(which(int_dys_diff <= 0))]
    } else {
      int_id_end <- length(int_dys)
    }
    int_dy_end <- int_dys[int_id_end]
    
    ## loop over 8-day intervals
    int_dys_seq <- seq(int_dy_start, int_dy_end, 8)
    ch_dys_seq <- formatC(int_dys_seq, width = 3, flag = 0)
    
    for (j in ch_dys_seq) {
      
      int_id_dy <- grep(j, ch_dys)
      ch_url_dy <- paste0(ch_url_yr, ch_dys[int_id_dy], "/")
      
      ## available tiles per current time step
      ch_tls <- getURL(ch_url_dy, verbose = FALSE, dirlistonly = TRUE, 
                       userpwd = "anonymous:")
      ls_tls <- strsplit(ch_tls, "\n")
      ch_tls <- unlist(ls_tls)
      
      ## identify and download target tile
      int_id_trgt <- grep(ch_tls_pttrn, ch_tls)
      ch_tls_trgt <- ch_tls[int_id_trgt]
      
      ch_url_tl <- paste0(ch_url_dy, ch_tls_trgt)
      ch_fl_tl <- basename(ch_url_tl)
      ch_fl_out <- paste0(ch_dir_out, ch_fl_tl)
      
      download.file(ch_url_tl, ch_fl_out, ...)
    }
    
  }
}