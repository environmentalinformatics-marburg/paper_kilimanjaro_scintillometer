### environmental stuff

## required packages and functions
library(rgdal)
library(OpenStreetMap)

## path: plot coordinates
ch_dir_crd <- "../../kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


### data processing

## gps data
source("R/mergeGPX.R")
spt_sensors <- mergeGPX(df2sp = TRUE)

## subset b-poles
sp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd, 
                  stringsAsFactors = FALSE, p4s = "+init=epsg:21037")
sp_plt <- subset(sp_plt, PlotID %in% c("cof2", "gra2") & PoleType == "B")
sp_plt_ll <- spTransform(sp_plt, CRS("+init=epsg:4326"))

## unique land covers
ch_plots <- unique(sp_plt_ll$PlotID)

for (i in ch_plots) {
  
  id <- grep(i, sp_plt_ll$PlotID)
  
  ## map extent
  ext <- extent(sp_plt_ll[id, ])
  ul <- c(ymax(ext) + .001, xmin(ext) - .001)
  lr <- c(ymin(ext) - .001, xmax(ext) + .001)
  
  ## get osm data
  osm_plt <- openmap(upperLeft = ul, lowerRight = lr, type = "bing", 
                     minNumTiles = 9L, zoom = 19)
  osm_plt_clrk <- openproj(osm_plt, "+init=epsg:21037")
  rst_plt_clrk <- raster(osm_plt_clrk)
  
  ## visualize b-poles
  spt_plot <- sp_plt_ll[id, ]
  
  p_bpoles <- autoplot(osm_plt_clrk) + 
    geom_point(aes(x = X, y = Y), data = spt_plot@data, size = 6, colour = "white") + 
    geom_point(aes(x = X, y = Y), data = spt_plot@data, size = 4, colour = "black")
  
  png(paste0("out/", i, ".png"), width = 600, res = 100)
  plotRGB(rst_plt_clrk)
  points(sp_plt_cntr, col = "red", cex = 2, pch = 20)
  points(sp_plt, col = "red", cex = 2, pch = 20)
  dev.off()
    
}