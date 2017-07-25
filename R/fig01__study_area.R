### environmental stuff -----

## packages and function
source("R/slsPkgs.R")
source("R/slsFcts.R")

## path for output storage
dir_prm <- switch(Sys.info()[["sysname"]], 
                  "Windows" = "E:/", "Linux" = "/media/permanent/")
dir_ppr <- paste0(dir_prm, "pub/papers/detsch_et_al__spotty_evapotranspiration/")

# path and file with research plot coordinates
dir_crd <- paste0(dir_prm, "data/kili/coordinates")
fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

dir_np <- paste0(dir_prm, "data/kili/kinapa")
spy_np <- readOGR(dir_np, "fdetsch-kilimanjaro-1420532792846", 
                  p4s = "+init=epsg:4326")


### data import -----

# research plots
spp_plt <- readOGR(dsn = dir_crd, layer = fls_crd, p4s = "+init=epsg:21037")
spp_plt_amp <- subset(spp_plt, PoleType == "AMP")
spp_plt_amp <- spTransform(spp_plt_amp, CRS = CRS("+init=epsg:4326"))  

int_plt_sls <- spp_plt_amp$PlotID %in% slsPlots()
spp_plt_amp_sls <- spp_plt_amp[int_plt_sls, ]


## visualization

# extent expansion
ext_plt_amp_sls <- extent(spp_plt_amp_sls)

num_xmin <- xmin(ext_plt_amp_sls) - .1
num_xmax <- xmax(ext_plt_amp_sls) + .05
num_xlim <- c(num_xmin, num_xmax)

num_ymin <- ymin(ext_plt_amp_sls) - .05
num_ymax <- ymax(ext_plt_amp_sls) + .1
num_ylim <- c(num_ymin, num_ymax)

## satellite image
rst_kili <- kiliAerial(c(num_ymax, num_xmin), c(num_ymin, num_xmax), 
                       lonlat = TRUE, scale = 2, type = "satellite", rgb = TRUE)

rst_kili2 <- kiliAerial(c(num_ymax, num_xmin), c(num_ymin, num_xmax), 
                        minNumTiles = 20L, type = "bing")

spl_kili <- rgb2spLayout(rst_kili, alpha = .8, quantiles = c(0, 1))

# point coordinates
mat_crd <- coordinates(spp_plt_amp_sls)

# bing aerial including point locations
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 0.08998809, 
              offset = c(37.16, -3.405), fill = c("transparent", "black"))
text1 = list("sp.text", c(37.16, -3.3925), "0", cex = .6, font = 2)
text2 = list("sp.text", c(37.257, -3.3925), "10 km", cex = .6, font = 2)

p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", 
                 scales = list(draw = TRUE, cex = .7), 
                 col.regions = "white", cex = 1.2, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = list(spl_kili, 
                                  # list("sp.text", loc = c(37.65, -3.4), 
                                  #      txt = "\uA9 OpenStreetMap contributors", 
                                  #      font = 2, cex = .5, col = "grey90"), 
                                  list("sp.text", loc = c(37.5, -3.05), 
                                       txt = "Kilimanjaro National Park", 
                                       font = 2, cex = .7, col = "grey50", 
                                       srt = -60),
                                  scale, text1, text2, 
                                  list("sp.lines", as(spy_np, "SpatialLines"), 
                                       col = "grey50"))) + 
  layer(sp.points(spp_plt_amp_sls, cex = .8, pch = 20, col = "black"))

# geographic context
p_cont <- visKili(cex = .75, lwd = .05, ext = rst_kili)

## photographs
img1 <- stack("../../phd/scintillometer/IMAG0235.jpg")
p1 <- spplot(img1[[1]], col.regions = "transparent", colorkey = FALSE,
             sp.layout = list(
               rgb2spLayout(img1), 
               list("sp.text", c(.075 * xmax(img1), .9 * ymax(img1)), "b)", cex = .8, font = 2)
             ))

img2 <- stack("../../phd/scintillometer/IMG_20141201_064715_187.jpg")
img2 <- crop(img2, img1)
p2 <- spplot(img2[[1]], col.regions = "transparent", colorkey = FALSE,
             sp.layout = list(
               rgb2spLayout(img2), 
               list("sp.text", c(.075 * xmax(img2), .9 * ymax(img2)), "c)", cex = .8, font = 2)
             ))

p <- latticeCombineGrid(list(p1, p2), between = list(y = .8))

# output filename
dir_ltx <- paste0(dir_ppr, "journals/ema/review/latex")
fls_out <- paste0(dir_ltx, "/img/figure01.tiff")


### visualization -----

## standalone .tiff figure
tiff(fls_out, width = 19, height = 12, units = "cm", res = 300, 
     compression = "lzw")

# bing image incl point locations
grid.newpage()
vp0 <- viewport(x = 0, y = 0, width = .6, height = 1, 
                just = c("left", "bottom"))
pushViewport(vp0)
print(p_bing, newpage = FALSE)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))
offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
               xlim = num_xlim, ylim = num_ylim, offset = .02,
               gp = gpar(fontsize = 8, fontfamily = "Helvetica", fill = "grey50"))

# explicitly name current viewport
vp1 <- viewport(x = 0, y = 0, width = 1, height = 1, just = c("left", "bottom"), 
                name = "vp1")
pushViewport(vp1)

# add topographic map
vp_cont <- viewport(x = .675, y = .625, just = c("left", "bottom"),
                    width = .375, height = .425)
pushViewport(vp_cont)
print(p_cont, newpage = FALSE)

# add equator label
downViewport(trellis.vpname("figure"))
grid.text(x = .05, y = .38, just = c("left", "bottom"), label = "Eq.",
          gp = gpar(cex = .4))

seekViewport("vp1")
vp2 <- viewport(x = 1.1, y = 0, width = .6, height = 1,
                just = c("left", "bottom"))
pushViewport(vp2)
print(p, newpage = FALSE)

dev.off()


### create subfigure a) in preparation for Impress template -----

p <- lapply(c("google.tif", "bing.tif"), function(i) {
  
  fls_kili <- paste0(dir_ppr, "/journals/ema/review/aerials/", i)
  rst_kili <- if (!file.exists(fls_kili)) {
    rst <- if (i == "google.tif") {
      kiliAerial(c(num_ymax, num_xmin), c(num_ymin, num_xmax), 
                 lonlat = TRUE, scale = 2, type = "satellite", rgb = TRUE)
    } else {
      kiliAerial(c(num_ymax, num_xmin), c(num_ymin, num_xmax), 
                 minNumTiles = 20L, type = "bing")
    }
    writeRaster(rst, fls_kili)
  } else {
    stack(fls_kili)
  }
  spl_kili <- rgb2spLayout(rst_kili, alpha = .9, quantiles = c(0, 1))
  
  img <- spplot(spp_plt_amp_sls, zcol = "PlotID", maxpixels = ncell(rst_kili),
                scales = list(draw = TRUE, cex = .7), 
                col.regions = "white", cex = 1.2, pch = 20,
                auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                sp.layout = list(spl_kili 
                                 # list("sp.text", loc = c(37.65, -3.4), 
                                 #      txt = "\uA9 OpenStreetMap contributors", 
                                 #      font = 2, cex = .5, col = "grey90"), 
                                 , list("sp.text", loc = c(37.49, -3.03), 
                                        txt = "Kilimanjaro National Park", 
                                        font = 2, cex = .6, col = "grey10", 
                                        srt = -57.5)
                                 , scale, text1, text2
                                 ,list("sp.lines", as(spy_np, "SpatialLines"),
                                       col = "grey10")
                )) + 
    layer(sp.points(spp_plt_amp_sls, cex = 1.2, pch = 20, col = "white")) + 
    layer(sp.points(spp_plt_amp_sls, cex = .8, pch = 20, col = "black"))
  
  # bing image incl point locations
  fls_out <- paste0(dir_ltx, "/img/Fig01a_", i, "f")
  tiff(fls_out, width = 15, height = 12, units = "cm", res = 500, 
       compression = "lzw")
  
  grid.newpage()
  print(img, newpage = FALSE)
  
  # insertion of shadow text
  downViewport(trellis.vpname(name = "figure"))
  offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
                 xlim = num_xlim, ylim = num_ylim, offset = .02,
                 gp = gpar(fontsize = 10, fontfamily = "Helvetica", fill = "grey50"))
  
  # explicitly name current viewport
  vp1 <- viewport(x = 0, y = 0, width = 1, height = 1, just = c("left", "bottom"), 
                  name = "vp1")
  pushViewport(vp1)
  
  # add topographic map
  vp_cont <- viewport(x = .675, y = .625, just = c("left", "bottom"),
                      width = .375, height = .425)
  pushViewport(vp_cont)
  print(p_cont, newpage = FALSE)
  
  # add equator label
  downViewport(trellis.vpname("figure"))
  grid.text(x = .05, y = .38, just = c("left", "bottom"), label = "Eq.",
            gp = gpar(cex = .5))
  
  dev.off()
  
  system(paste("convert -trim", fls_out, fls_out))
})
