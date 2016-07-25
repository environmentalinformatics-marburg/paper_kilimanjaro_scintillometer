### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
library(Orcs)
lib <- c("rgdal", "MODIS", "doParallel", "Rsenal", "latticeExtra")
loadPkgs(lib)

## functions
source("R/slsPlots.R")

slope <- function(x, y) {
  diff(y) / diff(x)
}

## set working directory
dir_old <- getwd()
setwdOS(path_lin = "/media/fdetsch/Permanent", path_win = "E:/", 
        path_ext = "phd/scintillometer")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


### data import -----

## whittaker-smoothed 500-m lai
fls_lai <- list.files("data/MCD15A2H.006/whittaker", 
                      pattern = "^MCD15A2H.*.tif$", full.names = TRUE)
dts_lai <- as.Date(extractDate(fls_lai, 11, 17)$inputLayerDates, "%Y%j")
rst_lai <- stack(fls_lai)
mat_lai <- as.matrix(rst_lai)
rst_lai_ltm <- setValues(rst_lai[[1]], apply(mat_lai, 1, mean))

## modeled 500-m wdrvi
fls_wdrvi <- list.files("data/MCD13A1.006/whittaker/wdrvi", 
                        pattern = "^MCD13A1.*.tif$", full.names = TRUE)
rst_wdrvi <- stack(fls_wdrvi)
mat_wdrvi <- as.matrix(rst_wdrvi)
rst_wdrvi_ltm <- setValues(rst_wdrvi[[1]], apply(mat_wdrvi, 1, mean))

## whittaker-smoothed 500-m ndvi
fls_ndvi <- list.files("data/MCD13A1.006/whittaker", 
                       pattern = "^MCD13A1.*.tif$", full.names = TRUE)
rst_ndvi <- stack(fls_ndvi)
mat_ndvi <- as.matrix(rst_ndvi)
rst_ndvi_ltm <- setValues(rst_ndvi[[1]], apply(mat_ndvi, 1, mean))


### performance of lai vs. wdrvi / ndvi

## data
lai <- rst_lai_ltm[]; ndvi <- rst_ndvi_ltm[]; wdrvi <- rst_wdrvi_ltm[]

dat <- data.frame(lai = lai, ndvi = ndvi, wdrvi = wdrvi)
dat <- dat[order(dat$lai), ]
dat <- dat[complete.cases(dat), ]

## logarithmic models
m1 <- lm(wdrvi ~ log(lai), data = dat)
m2 <- lm(ndvi ~ log(lai), data = dat)

x <- seq(.01, 6, .01)
y1 <- predict(m1, newdata = data.frame(lai = x))
y2 <- predict(m2, newdata = data.frame(lai = x))

z1 <- x[which((slope(x, y1) < 0.06))[1]]
z2 <- x[which((slope(x, y2) < 0.06))[1]]

## linear models
m3 <- lm(wdrvi ~ lai, data = subset(dat, lai < 4))
m4 <- lm(ndvi ~ lai, data = subset(dat, lai < 4))

## visualize
p1 <- xyplot(wdrvi ~ lai, data = dat, panel = function(x, y, ...) {
  panel.xyplot(x, y, col = "grey", ...)
  
  m1 <- lm(y ~ log(x))
  panel.lines(x, predict(m1), col = "red", lwd = 1.5)
  
  panel.abline(v = z1, lty = 2, lwd = 1.5)
  panel.text(x = z1, y = min(y) + .2, labels = paste("Threshold saturation:", z1), 
             srt = 90, adj = c(.4, 1.5), font = 2)
  
  panel.ablineq(lm(wdrvi ~ lai, data = subset(dat, lai < 4)), 
                rotate = TRUE, r.squared = TRUE, col.line = "red", lty = 2, 
                col.text = "transparent")
})

p2 <- xyplot(ndvi ~ lai, data = dat, panel = function(x, y, ...) {
  panel.xyplot(x, y, col = "grey", ...)
  
  m2 <- lm(y ~ log(x))
  panel.lines(x, predict(m2), col = "red", lwd = 1.5)
  
  panel.abline(v = z2, lty = 2, lwd = 1.5)
  panel.text(x = z2, y = min(y) + .2, labels = paste("Threshold saturation:", z2), 
             srt = 90, adj = c(.525, 1.5), font = 2)
  
  panel.ablineq(lm(ndvi ~ lai, data = subset(dat, lai < 4)), 
                rotate = TRUE, r.squared = TRUE, col.line = "red", lty = 2, 
                col.text = "transparent")
})


### performance of modeled lai -----
## modeled 250-m lai
fls_lai250 <- list.files("data/MCD13Q1.006/whittaker", 
                      pattern = "^MCD13Q1.*LAI.tif$", full.names = TRUE)
dts_lai250 <- as.Date(extractDate(fls_lai250)$inputLayerDates, "%Y%j")
rst_lai250 <- stack(fls_lai250)
mat_lai250 <- as.matrix(rst_lai250)

## sls plots and files
spt_plt <- readOGR("../../kilimanjaro/coordinates", 
                   "PlotPoles_ARC1960_mod_20140807_final", 
                   p4s = "+init=epsg:21037")
spt_plt <- subset(spt_plt, PoleType == "AMP" & PlotID %in% slsPlots())


### value extraction -----

## extract values per plot
dat_eval <- foreach(i = 1:length(spt_plt), .combine = "rbind") %do% {
  
  # 1-km data
  plt <- subset(spt_plt, PlotID == spt_plt$PlotID[i])
  cell_lai <- cellFromXY(rst_lai, plt)
  val_lai <- mat_lai[cell_lai, ]
  
  # long-term mean lai and wdrvi
  ltm_lai <- mean(val_lai)
  
  val_wdrvi <- mat_wdrvi[cell_lai, ]
  ltm_wdrvi <- mean(val_wdrvi)

  # mean 250-m data
  rst <- rst_lai[[1]]
  rst[][-cell_lai] <- NA
  shp <- rasterToPolygons(rst)
  cells_lai250 <- unlist(cellFromPolygon(rst_lai250, shp))
  val_lai250 <- mat_lai250[cells_lai250, ]
  val_lai250 <- colMeans(val_lai250)

  # statistics  
  mod <- lm(val_lai250 ~ val_lai)
  p <- pvalue(mod)
  
  stats <- regressionStats(val_lai250, val_lai)
  
  data.frame(PlotID = plt@data$PlotID, LAI = ltm_lai, WDRVI = ltm_wdrvi, 
             Rsq = stats$Rsq, p = p, IOA = ioa(val_lai250, val_lai),
             RMSE = stats$RMSE, RMSE.se = stats$RMSE.se)
}
