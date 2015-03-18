slsMergeDailyData <- function(files, 
                              ...) {
  
  lib <- c("foreach", "plyr")
  sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))
  
  if (length(grep("gra1", files)) > 0) {
    int_id_20140224 <- grep("2014-02-24", files)
    files <- files[-int_id_20140224]
  }
  
  dat <- foreach(i = files, j = seq(files)) %do% {
    
    # Import headers
    hdr <- readLines(i)
    index <- grep("#", hdr)
    hdr <- hdr[index]
    hdr <- sapply(strsplit(hdr, " # "), "[[", 2)
    
    # Import data
    tmp <- read.table(i, header = FALSE, skip = index[length(index)] + 1, 
                      sep = "\t", na.strings = "*", stringsAsFactors = FALSE)
    names(tmp) <- hdr
    
    tmp <- tmp[2:nrow(tmp), ]
    
    # Reformat datetime 
    tmp.date <- substr(tmp[, 1], 13, 22)
    tmp.time <- substr(tmp[, 1], 24, 31)
    tmp$datetime <- strftime(strptime(paste(tmp.date, tmp.time), 
                                      format = "%Y-%m-%d %H:%M:%S"))
    
    return(tmp)
  }
  
  dat <- rbind.fill(dat)
  
  # Remove rows with time == NA
  if (length(which(is.na(dat$time))) > 0)
    dat <- dat[-which(is.na(dat$time)), ]
  # Order data by datetime
  dat <- dat[order(dat$datetime), ]
  
  # Continuous time series(i.e., begin 0:00 -> end 23:59)
  st <- paste(substr(dat$datetime[1], 1, 10), "00:00:00")
  nd <- paste(substr(dat$datetime[nrow(dat)], 1, 10), "23:59:00")
  
  st.nd <- seq(strptime(st, format = "%Y-%m-%d %H:%M:%S"), 
               strptime(nd, format = "%Y-%m-%d %H:%M:%S"), 60)
  st.nd <- strftime(st.nd, format = "%Y-%m-%d %H:%M:%S")
  
  dat <- merge(data.frame(datetime = st.nd), dat, all.x = TRUE)
  
  # Return merged and continuous time series
  return(dat)
}