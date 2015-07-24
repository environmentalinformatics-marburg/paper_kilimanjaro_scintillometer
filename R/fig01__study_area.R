## environmental stuff

# packages
source("R/slsPkgs.R")
source("R/slsFcts.R")

# path for output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# path and file with research plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data

# bing aerial image
rst_kili <- kiliAerial(minNumTiles = 20, rasterize = TRUE)
spl_kili <- rgb2spLayout(rst_kili, alpha = .8)

# research plots
spp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd, 
                   p4s = "+init=epsg:21037")
spp_plt_amp <- subset(spp_plt, PoleType == "AMP")

int_plt_sls <- spp_plt_amp$PlotID %in% slsPlots()
spp_plt_amp_sls <- spp_plt_amp[int_plt_sls, ]


## visualization

# extent expansion
ext_plt_amp_sls <- extent(spp_plt_amp_sls)

num_xmin <- xmin(ext_plt_amp_sls) - 10000
num_xmax <- xmax(ext_plt_amp_sls) + 5000
num_xlim <- c(num_xmin, num_xmax)

num_ymin <- ymin(ext_plt_amp_sls) - 5000
num_ymax <- ymax(ext_plt_amp_sls) + 10000
num_ylim <- c(num_ymin, num_ymax)

# point coordinates
mat_crd <- coordinates(spp_plt_amp_sls)

# bing aerial including point locations
p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", 
                 scales = list(draw = TRUE, cex = 1.25), 
                 col.regions = "white", cex = 1.75, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = spl_kili) + 
  layer(sp.points(spp_plt_amp_sls, cex = 1.25, pch = 20, col = "black"))

# geographic context
p_cont <- visKili()

# output filename
ch_fls_out <- paste0(ch_dir_ppr, "fig/fig01__study_area.png")

# figure
png(ch_fls_out, width = 25, height = 24, units = "cm", pointsize = 18, res = 300)

# bing image incl point locations
grid.newpage()
print(p_bing)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))

offsetGridText(x = mat_crd, labels = spp_plt_amp_sls$PlotID, stext = TRUE,
               xlim = num_xlim, ylim = num_ylim, offset = .0175, 
               gp = gpar(fontsize = 25, fontfamily = "Bookman Old Style"))

vp_cont <- viewport(x = .675, y = .625, just = c("left", "bottom"), 
                    width = .3, height = .35)
pushViewport(vp_cont)
print(p_cont, newpage = FALSE)

dev.off()
