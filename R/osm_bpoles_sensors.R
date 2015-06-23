### environmental stuff

## required packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## path: plot coordinates
ch_dir_crd <- "../../kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

## path: output storage
ch_dir_out <- "../../phd/scintillometer/out/osm/"

## crs: `openmap`
ch_epsg_sphmrc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"

### data processing

## gps data
spt_sls_ll <- mergeGPX(df2sp = TRUE)
spt_sls <- mergeGPX(df2sp = TRUE, CRS = ch_epsg_sphmrc)
spt_sls <- subset(spt_sls, season == "W")

## subset b-poles
sp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd, 
                  stringsAsFactors = FALSE, p4s = "+init=epsg:21037")
sp_plt <- subset(sp_plt, PlotID %in% slsPlots() & PoleType %in% c("B", "AMP"))
sp_plt_ll <- spTransform(sp_plt, CRS("+init=epsg:4326"))
sp_plt_sphmrc <- spTransform(sp_plt, CRS = ch_epsg_sphmrc)
df_plt_sphmrc_crd <- data.frame(coordinates(sp_plt_sphmrc))
names(df_plt_sphmrc_crd) <- c("X", "Y")

## unique land covers
ch_plots <- unique(sp_plt_ll$PlotID)

ls_p_osm <- lapply(ch_plots, function(i) {

  ## subset shapefile (ll for `openmap`, sphmrc for visualization)
  id <- grep(i, sp_plt_ll$PlotID)
  spt_plot <- sp_plt_ll[id, ]
  if (any(spt_plot$PoleType == "B"))
    spt_plot <- subset(spt_plot, PoleType == "B")
  
  df_plot_sphmrc_crd <- df_plt_sphmrc_crd[id, ]
  
  ## subset gps data
  id_gps <- grep(toupper(i), spt_sls_ll$plot)
  spt_sls_ll_plot <- spt_sls_ll[id_gps, ]
  #   plotKML(spt_sls_ll_plot, 
  #           file.name = paste0("data/gps/gps_", i, ".kml"), open.kml = FALSE)
  
  ## extend map extent omnidirectionally
  ext <- extendByDist(extent(spt_plot), width = .00075)

  ## if plot extent and sls setup do not intersect, 
  ## extend towards selected directions
  spy_ext <- as(ext, "SpatialPolygons")
  proj4string(spy_ext) <- proj4string(spt_plot)
  
  while (!gContains(spy_ext, spt_sls_ll_plot)) {
    ext_sls <- extent(spt_sls_ll_plot)
    
    left <- !xmin(ext_sls) > xmin(ext) 
    right <- !xmax(ext_sls) < xmax(ext)
    bottom <- !ymin(ext_sls) > ymin(ext) 
    top <- !ymax(ext_sls) < ymax(ext)
    
    num_dist <- rep(0, 4)
    log_dist <- c(bottom, left, top, right)
    num_dist[log_dist] <- .001
    ext <- extendByDist(ext, width = num_dist)
    
    spy_ext <- as(ext, "SpatialPolygons")
    proj4string(spy_ext) <- proj4string(spt_plot)
  }
  
  ## map extent
  ul <- c(ymax(ext), xmin(ext))
  lr <- c(ymin(ext), xmax(ext))
  
  ## get osm data
  osm_plt <- openmap(upperLeft = ul, lowerRight = lr, type = "bing", 
                     minNumTiles = 4L, zoom = 19)

  ## subset and reproject gps data
  id_gps <- grep(toupper(i), spt_sls$plot)
  spt_sls_plot <- spt_sls[id_gps, ]
  df_sls_plot_crd <- data.frame(coordinates(spt_sls_plot))
  
  ## visualize b-poles
  p_bpoles <- autoplot(osm_plt) + 
    geom_point(aes(x = X, y = Y), data = df_plot_sphmrc_crd, size = 6, colour = "white") + 
    geom_point(aes(x = X, y = Y), data = df_plot_sphmrc_crd, size = 4, colour = "black") + 
    geom_point(aes(x = lon, y = lat), data = df_sls_plot_crd, shape = 17, 
               size = 6, colour = "white") + 
    geom_point(aes(x = lon, y = lat), data = df_sls_plot_crd, shape = 17, 
               size = 4, colour = "red") + 
    labs(x = "", y = "", title = paste0(toupper(i), "\n")) +
    theme(text = element_text(size = 14))
  
  png(paste0(ch_dir_out, i, "_osm.png"), width = 20, height = 20, units = "cm", 
      res = 300, pointsize = 18)
  print(p_bpoles)
  dev.off()
  
  return(p_bpoles)
})