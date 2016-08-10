### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
library(Orcs)
lib <- c("rgdal", "MODIS", "foreach", "Rsenal", "latticeExtra", "grid", 
         "stargazer")
Orcs::loadPkgs(lib)

## functions
source("R/slsPlots.R")

slope <- function(x, y) {
  diff(y) / diff(x)
}

## set working directory
dir_old <- getwd()
setwdOS(path_lin = "/media/fdetsch/Permanent", path_win = "E:/", 
        path_ext = "phd/scintillometer")


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


### performance of lai vs. wdrvi / ndvi -----

## data
lai <- rst_lai_ltm[]; ndvi <- rst_ndvi_ltm[]; wdrvi <- rst_wdrvi_ltm[]

dat <- data.frame(lai = lai, ndvi = ndvi, wdrvi = wdrvi)
dat <- dat[order(dat$lai), ]
dat <- dat[complete.cases(dat), ]

## logarithmic models
m1 <- lm(wdrvi ~ log(lai), data = dat)
m2 <- lm(ndvi ~ log(lai), data = dat)

x <- seq(0, 6, .01)
y1 <- predict(m1, newdata = data.frame(lai = x))
y2 <- predict(m2, newdata = data.frame(lai = x))

z1 <- x[which((slope(x, y1) < 0.06))[1]]
z2 <- x[which((slope(x, y2) < 0.06))[1]]

## linear models
m3 <- lm(wdrvi ~ lai, data = subset(dat, lai < 4))
m4 <- lm(ndvi ~ lai, data = subset(dat, lai < 4))

## lai at which linear correlation is best
Rsq3 <- do.call("rbind", lapply(seq(2, 8, .01), function(i) {
  rsq <- summary(lm(wdrvi ~ lai, data = subset(dat, lai <= i)))$r.squared
  data.frame(LAI = i, Rsq = rsq)
}))
max3 <- Rsq3[which.max(Rsq3$Rsq), 1]

Rsq4 <- do.call("rbind", lapply(seq(2, 8, .01), function(i) {
  rsq <- summary(lm(ndvi ~ lai, data = subset(dat, lai <= i)))$r.squared
  data.frame(LAI = i, Rsq = rsq)
}))
max4 <- Rsq4[which.max(Rsq4$Rsq), 1]

## visualize
cols <- colorRampPalette(c("white", brewer.pal(5, "YlOrRd")))
cols <- colorRampPalette(c("white", envinmrPalette(7)))

p1 <- xyplot(wdrvi ~ lai, data = dat, panel = function(x, y, ...) {
  panel.smoothScatter(x, y, colramp = cols, nbin = 250, nrpoints = 0)
  
  m1 <- lm(y ~ log(x))
  panel.lines(x, predict(m1), col = "black", lwd = 2)
  
  panel.abline(v = z1, lty = 3, lwd = 1.5, col = "grey35")
  panel.text(x = z1 + .3, y = min(y) + .15, labels = paste("T.s. =", z1), 
             srt = 90, font = 2, cex = .8, col = "grey35")
  
  panel.ablineq(lm(wdrvi ~ lai, data = subset(dat, lai <= max3)),
                rotate = TRUE, r.squared = TRUE, col.line = "black", lty = 2,
                col.text = "transparent", lwd = 2)
}, xlab = list(expression("LAI"[500]), cex = .9), 
ylab = list("WDRVI", cex = .9), scales = list(cex = .85))

p2 <- xyplot(ndvi ~ lai, data = dat, panel = function(x, y, ...) {
  panel.smoothScatter(x, y, colramp = cols, nbin = 250, nrpoints = 0)
  
  m2 <- lm(y ~ log(x))
  panel.lines(x, predict(m2), col = "black", lwd = 2)
  
  panel.abline(v = z2, lty = 3, lwd = 1.5, col = "grey35")
  panel.text(x = z2 + .3, y = min(y) + .10875, labels = paste("T.s. =", z2), 
             srt = 90, font = 2, cex = .8, col = "grey35")
  
  panel.ablineq(lm(ndvi ~ lai, data = subset(dat, lai <= max4)),
                rotate = TRUE, r.squared = TRUE, col.line = "black", lty = 2,
                col.text = "transparent", lwd = 2)
}, xlab = list(expression("LAI"[500]), cex = .9), ylab = list("NDVI", cex = .9), 
scales = list(cex = .85))

## write to file
setEPS()
postscript("out/figure_0x.eps", width = 19*.3937, height = 9*.3937, 
           fonts = c("serif", "Palatino"))
grid.newpage()

vp0 <- viewport(x = 0, y = 0, width = .5, height = 1, 
                just = c("left", "bottom"))
pushViewport(vp0)
print(p2, newpage = FALSE)
downViewport(trellis.vpname("figure"))
grid.text("a)", x = .05, y = .915, just = c("left", "bottom"), 
          gp = gpar(cex = .8, fontface = "bold"))

upViewport(0)
vp1 <- viewport(x = .5, y = 0, width = .5, height = 1, 
                just = c("left", "bottom"))
pushViewport(vp1)
print(p1, newpage = FALSE)
downViewport(trellis.vpname("figure"))
grid.text("b)", x = .05, y = .915, just = c("left", "bottom"), 
          gp = gpar(cex = .8, fontface = "bold"))

dev.off()


### performance of modeled lai -----

## import modeled 250-m lai
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
  
  stats <- regressionStats(val_lai250, val_lai, adj.rsq = FALSE)
  
  data.frame(PlotID = plt@data$PlotID, LAI500 = ltm_lai, SD = sd(val_lai), 
             LAI250 = mean(val_lai250), Rsq = stats$Rsq, p = p, 
             RMSE = stats$RMSE, RMSE.se = stats$RMSE.se, 
             IOA = ioa(val_lai250, val_lai))
}

## round values
tbl <- dat_eval
tbl[, c(2:5, 7, 9)] <- round(tbl[, c(2:5, 7, 9)], 2)

## merge r-squared with significance
id3 <- tbl$p < .001; tbl$Rsq[id3] <- paste0(tbl$Rsq[id3], "***")
id2 <- tbl$p >= .001 & tbl$p < .01; tbl$Rsq[id2] <- paste0(tbl$Rsq[id2], "**")
id1 <- tbl$p >= .01 & tbl$p < .05; tbl$Rsq[id1] <- paste0(tbl$Rsq[id1], "*")
tbl <- tbl[, -6]

## merge rmse with standard errors
tbl$RMSE.se <- round(tbl$RMSE.se, 3)
tbl$RMSE <- paste(tbl$RMSE, tbl$RMSE.se, sep = " \u00b1 ")
tbl <- tbl[, -7]

## reorder data
plt <- rev(slsPlots("elevation"))
tbl <- tbl[match(plt, tbl$PlotID), ]

## create output table
stargazer(tbl, summary = FALSE, rownames = FALSE, digits = NA, 
          decimal.mark = ".")
