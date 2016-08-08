### environmental stuff -----

## functions
source("R/slsPkgs.R")
source("R/slsPlots.R")


### processing -----

## tree inventory
dat <- read.table("../../phd/scintillometer/data/SP4_tree_inventory/10640.txt", 
                  header = TRUE, sep = "\t")

dat <- dat[, c("PLOTID", "HEIG")]

## average tree height per plot
plt <- slsPlots("elevation")

dat_ave <- foreach(i = plt, .combine = "rbind") %do% {
  dat_sub <- subset(dat, PLOTID == i)
  
  dat_sub$HEIG <- gsub(",", "\\.", dat_sub$HEIG)
  dat_sub$HEIG <- as.numeric(dat_sub$HEIG)
  
  data.frame(PlotID = i, Height = mean(dat_sub$HEIG, na.rm = TRUE))
}