## environmental stuff

# packages
lib <- c("foreach", "plyr", "ggplot2", "reshape2")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# srun path
ch_dir_srun <- "../../SRun/"
srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# setwd(srunWorkspaces[11])
# 
# source("../../phd/scintillometer/src/tsOutliers.R")


## data

ch_fls <- list.files(paste0(srunWorkspaces[3], "/data/retrieved_SPU-111-230"), 
                     pattern = ".mnd$", full.names = TRUE)

dat <- foreach(i = ch_fls, j = seq(ch_fls)) %do% {
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

tst <- do.call("rbind", dat)
# tst <- rbind.fill(dat)
tst <- tst[order(tst$datetime), ]

# Retrieve error messages
sort(table(tst$error))

# Merge continuous with Scintillometer time series
time.seq <- strptime(tst$datetime, format = "%Y-%m-%d %H:%M:%S")
time.seq <- strftime(seq(min(time.seq), max(time.seq), 60), 
                     format = "%Y-%m-%d %H:%M:%S")
tst2 <- merge(data.frame(datetime = time.seq), 
              tst[, c("datetime", "latHeatFlux", "waterET", "error", "soilHeatFlux")], by = 1, all.x = TRUE)
tst2$datetime <- strptime(tst2$datetime, format = "%Y-%m-%d %H:%M:%S")

# Plot data sampled every minute
ggplot(aes(x = datetime, y = waterET), data = tst2) + 
  geom_line() + 
  geom_point()  

# tst2$latHeatFlux[tsOutliers(tst2$latHeatFlux, index = TRUE, 
#                             lower.limit = .3)] <- NA

tst3 <- merge(data.frame(datetime = time.seq), 
              tst[, c("datetime", "waterET", "temp", "humidity", 
                      "dwnRad", "upwRad", "precipRate", "error")], 
              by = 1, all.x = TRUE)
tst3$datetime <- strptime(tst2$datetime, format = "%Y-%m-%d %H:%M:%S")
# write.csv(tst3, "param_err.csv", row.names = FALSE)
tst3$datetime <- factor(as.character(tst3$datetime))
tst3.mlt <- melt(tst3, id.vars = 1)
tst3.mlt$datetime <- strptime(tst3.mlt$datetime, format = "%Y-%m-%d %H:%M:%S")

ggplot(aes(x = datetime, y = value), 
       data = subset(tst3.mlt, variable != "error")) + 
  geom_line() + 
  geom_point(size = .1) + 
  facet_wrap(~ variable, ncol = 2, scales = "free")

