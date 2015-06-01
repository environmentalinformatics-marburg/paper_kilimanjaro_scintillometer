slsMergeDailyData <- function(files, skip = 3, ...) {
  
  ## required packages
  source("R/slsPkgs.R")
  
  ## remove day-1 files from gra1
  if (length(grep("gra1", files)) > 0) {
    int_id_20140224 <- grep("2014-02-24", files)
    files <- files[-int_id_20140224]
  }
  
  ## remove day-1 files from sav5 (dry season)
  if (length(grep("sav5", files)) > 0 & length(grep("2014-09", files)) > 0) {
    int_id_20140912 <- grep("2014-09-12", files)[1]
    files <- files[int_id_20140912:length(files)]
  }
  
  ## import and merge data
  ls_df <- foreach(i = files) %do% {
    
    ## import headers
    hdr <- readLines(i)
    index <- grep("#", hdr)
    hdr <- hdr[index]
    hdr <- sapply(strsplit(hdr, " # "), "[[", 2)
    
    ## import data without headers
    tmp <- read.table(i, header = FALSE, skip = index[length(index)] + 1, 
                      sep = "\t", na.strings = "*", stringsAsFactors = FALSE)
    names(tmp) <- hdr
    
    ## skip the first 5 min of measurement, 
    ## or return NULL if measurement was shorter than 5 min
    if (nrow(tmp) <= skip)
      return(NULL)
    else 
      tmp <- tmp[(skip + 1L):nrow(tmp), ]
    
    ## reformat datetime 
    tmp.date <- substr(tmp[, 1], 13, 22)
    tmp.time <- substr(tmp[, 1], 24, 31)
    tmp$datetime <- strftime(strptime(paste(tmp.date, tmp.time), 
                                      format = "%Y-%m-%d %H:%M:%S"))
    
    return(tmp)
  }
  df <- rbind.fill(ls_df)
  
  ## remove rows with missing timestamp
  if (length(which(is.na(df$time))) > 0)
    df <- df[-which(is.na(df$time)), ]
  
  ## order by datetime
  df <- df[order(df$datetime), ]
  
  ## adjust sensor time on sav5 (dry season)
  if (length(grep("sav5", files)) > 0 & length(grep("2014-09", files)) > 0) {
    tmp_dt <- strptime(df$datetime, format = "%Y-%m-%d %H:%M:%S")
    tmp_dt$hour <- tmp_dt$hour + 1
    tmp_ch <- as.character(tmp_dt)
    df$datetime <- tmp_ch
  }
  
  ## merge data with continuous time series (begin 0:00, end 23:59) and return
  st <- paste(substr(df$datetime[1], 1, 10), "00:00:00")
  nd <- paste(substr(df$datetime[nrow(df)], 1, 10), "23:59:00")
  
  st.nd <- seq(strptime(st, format = "%Y-%m-%d %H:%M:%S"), 
               strptime(nd, format = "%Y-%m-%d %H:%M:%S"), 60)
  st.nd <- strftime(st.nd, format = "%Y-%m-%d %H:%M:%S")
  
  df <- merge(data.frame(datetime = st.nd), df, all.x = TRUE)

  return(df)
}