### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
library(Orcs)
lib <- c("rgdal", "MODIS", "foreach")
loadPkgs(lib)

## functions
source("R/slsAvlFls.R")
source("R/slsGppIndex.R")
source("R/slsPlots.R")
source("R/slsTemporalRange.R")

## set working directory
dir_old <- getwd()
setwdOS(path_lin = "/media/fdetsch/Permanent", path_win = "E:/", 
        path_ext = "phd/scintillometer")


### data import -----

## sls plots and files
spt_plt <- readOGR("../../kilimanjaro/coordinates", 
                   "PlotPoles_ARC1960_mod_20140807_final", 
                   p4s = "+init=epsg:21037")
spt_plt <- subset(spt_plt, PoleType == "AMP" & PlotID %in% slsPlots())

fls_sls <- slsAvlFls()
dts_sls <- slsTemporalRange(fls_sls)

## whittaker-smoothed ndvi
fls_lai <- list.files("data/MCD13Q1.006/whittaker", 
                      pattern = "^MCD13Q1.*LAI.tif$", full.names = TRUE)
fls_lai <- fls_lai[grep("2014001", fls_lai):grep("2014361", fls_lai)]
dts_lai <- as.Date(extractDate(fls_lai)$inputLayerDates, "%Y%j")
rst_lai <- stack(fls_lai)
mat_lai <- as.matrix(rst_lai)


### value extraction -----

## extract values per plot
dat_xtr <- foreach(i = 1:nrow(fls_sls), .combine = "rbind") %do% {
  
  # current plot and containing cell
  plt <- subset(spt_plt, PlotID == fls_sls$plot[i])
  cell <- cellFromXY(rst_lai, plt)
  
  # extract cell values
  val <- mat_lai[cell, ]
  nms <- names(val)
  mat <- matrix(val, 1, byrow = TRUE)

  dat <- data.frame(mat)
  names(dat) <- nms
  
  data.frame(PlotID = fls_sls$plot[i], season = fls_sls$season[i], dat)
}

dat_prm <- foreach(i = 1:nrow(dts_sls), .combine = "rbind") %do% {
  
  # current plot, season and time frame
  plt <- dts_sls$plot[i]; ssn <- dts_sls$season[i]
  dat <- subset(dat_xtr, PlotID == plt & season == ssn)
  
  # identify scene(s) corresponding to sls measurement
  dts <- subset(dts_sls, plot == plt & season == ssn)
  dts_id <- slsGppIndex(df_sls = dts, dt_fls_gpp = dts_lai)
  
  dat <- data.frame(dts, dat[, dts_id])
  names(dat)[1] <- "PlotID"
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(dat) > 5) {
    val <- rowMeans(dat[5:ncol(dat)], na.rm = TRUE)
    dat <- data.frame(dat[, 1:4], val)
  }
  
  # mean value
  data.frame(dat[1, 1:4], value = dat[, 5])
}

## write to file
saveRDS(dat_prm, paste0(dir_old, "/data/modis_lai250.rds"))
