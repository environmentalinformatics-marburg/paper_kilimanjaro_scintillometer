## environmental stuff

# packages
source("R/slsPkgs.R")
source("R/slsFcts.R")

# path for output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# path and file with research plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

ch_dir_np <- "../../kilimanjaro/kinapa/"
spy_np <- readOGR(ch_dir_np, "fdetsch-kilimanjaro-1420532792846", 
                  p4s = "+init=epsg:4326")


## data

# bing aerial image
rst_kili <- kiliAerial(minNumTiles = 20, projection = "+init=epsg:4326")
spl_kili <- rgb2spLayout(rst_kili, alpha = .8, quantiles = c(.005, .979))

# research plots
spp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd, 
                   p4s = "+init=epsg:21037")
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

# point coordinates
mat_crd <- coordinates(spp_plt_amp_sls)

# bing aerial including point locations
scale <- list("SpatialPolygonsRescale", layout.scale.bar(), scale = 0.08998809, 
              offset = c(37.175, -3.405), fill = c("transparent", "black"))
text1 = list("sp.text", c(37.175, -3.3925), "0", cex = .5, font = 2)
text2 = list("sp.text", c(37.265, -3.3925), "10 km", cex = .5, font = 2)

p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", 
                 scales = list(draw = TRUE, cex = .7), 
                 col.regions = "white", cex = 1.2, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = list(spl_kili, 
                                  list("sp.text", loc = c(37.65, -3.4), 
                                       txt = "\uA9 OpenStreetMap contributors", 
                                       font = 2, cex = .5, col = "grey90"), 
                                  scale, text1, text2, 
                                  list("sp.lines", as(spy_np, "SpatialLines"), 
                                       col = "grey50"))) + 
  layer(sp.points(spp_plt_amp_sls, cex = .8, pch = 20, col = "black"))

# geographic context
p_cont <- visKili(cex = .75, lwd = .05, ext = rst_kili)

# output filename
ch_fls_out <- paste0(ch_dir_ppr, "fig/figure01.png")

# figure
png(ch_fls_out, width = 15, height = 12, units = "cm", res = 500)

# bing image incl point locations
grid.newpage()
print(p_bing)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))

offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
               xlim = num_xlim, ylim = num_ylim, offset = .02, 
               gp = gpar(fontsize = 12, fontfamily = "Helvetica", fill = "grey50"))

vp_cont <- viewport(x = .675, y = .625, just = c("left", "bottom"), 
                    width = .375, height = .425)
pushViewport(vp_cont)
print(p_cont, newpage = FALSE)

# add equator label
downViewport(trellis.vpname("figure"))
grid.text(x = .05, y = .38, just = c("left", "bottom"), label = "Eq.", 
          gp = gpar(cex = .5))

dev.off()

## standalone tif version
ch_fls_tif <- paste0(ch_dir_ppr, "fig/figure01.tiff")

# figure
tiff(ch_fls_tif, width = 15, height = 12, units = "cm", res = 500, 
     compression = "lzw")

# bing image incl point locations
grid.newpage()
print(p_bing)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))

offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
               xlim = num_xlim, ylim = num_ylim, offset = .02, 
               gp = gpar(fontsize = 12, fontfamily = "Helvetica", fill = "grey50"))

vp_cont <- viewport(x = .675, y = .625, just = c("left", "bottom"), 
                    width = .375, height = .425)
pushViewport(vp_cont)
print(p_cont, newpage = FALSE)

# add equator label
downViewport(trellis.vpname("figure"))
grid.text(x = .05, y = .38, just = c("left", "bottom"), label = "Eq.", 
          gp = gpar(cex = .5))

dev.off()
