library(MODIS)
library(plotKML)

## plot center coordinates
spt_plots <- readOGR(dsn = "../../kilimanjaro/coordinates/coords/", 
                     layer = "PlotPoles_ARC1960_mod_20140807_final", 
                     p4s = "+init=epsg:21037")
spt_plots <- spt_plots[spt_plots$PoleType == "AMP", ]

## aspect from dem
rst_dem <- raster("../../kilimanjaro/coordinates/coords/DEM_ARC1960_30m_Hemp.tif")
rst_asp <- terrain(rst_dem, opt = "aspect", unit = "degrees")

## modis data
ch_dir_arc <- "/media/permanent/phd/scintillometer/data/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

MODISoptions(localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)
runGdal("MOD15A2", begin = "2015-01-01", end = "2015-01-02", tileH = 21, 
        tileV = 9, SDSstring = "010000", job = "cell_size", 
        outProj = "+init=epsg:21037")

## 1-km cells
ch_dir_cs <- paste(getOption("MODIS_outDirPath"), "cell_size", sep = "/")
ch_fls_cs <- list.files(ch_dir_cs, pattern = "Lai", full.names = TRUE)
rst_cs <- raster(ch_fls_cs)
rst_cs_crp <- crop(rst_cs, raster("../../kilimanjaro/kili_bing.tif"))
rst_tmp <- rst_cs_crp

for (i in slsPlots()) {
  spt_plots_sub <- subset(spt_plots, PlotID == i)
  int_cell <- cellFromXY(rst_tmp, spt_plots_sub)
  rst_tmp[][int_cell] <- 1
  rst_tmp[][-int_cell] <- NA
  spy_cs_sub <- rasterToPolygons(rst_tmp)
  
  ch_fls_kml <- paste0("data/", i, "_1km.kml")
  plotKML(spy_cs_sub, open.kml = FALSE, file.name = ch_fls_kml)
  
  # aspect
  print(paste(i, extract(rst_asp, spt_plots_sub), sep = ", "))
}

