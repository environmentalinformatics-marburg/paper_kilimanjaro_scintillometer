## environmental stuff

# packages
library(Rsenal)

# path for output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# path and file with research plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data

# plots included in sls field campaign
ch_plt_sls <- c("sav5", "sav4", 
                "mai4", "mai1", 
                "cof3", "cof2", 
                "gra1", "gra2", 
                "fer0", "fed1", 
                "hel1")

# bing aerial image
osm_kili <- kiliAerial(minNumTiles = 40)
rst_kili <- raster(osm_kili)
spl_kili <- rgb2spLayout(rst_kili)

# research plots
spp_plt <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd)
spp_plt_amp <- subset(spp_plt, PoleType == "AMP")

int_plt_sls <- spp_plt_amp$PlotID %in% ch_plt_sls
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

# label placement
mat_crd <- coordinates(spp_plt_amp_sls)
int_loc_lbl <- thigmophobe(mat_crd)

# bing aerial including point locations
p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", scales = list(draw = TRUE), 
                 col.regions = "grey65", cex = 1.5, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = spl_kili)

# text annotations
p_text <- layer(sp.text(loc = mat_crd, 
                        txt = spp_plt_amp_sls@data$PlotID, 
                        col = "grey65", cex = 2, pos = int_loc_lbl, offset = .5))

# output filename
ch_fls_out <- paste0(ch_dir_ppr, "fig/fig01__study_area.png")

# figure
png(ch_fls_out, width = 30, height = 28, units = "cm", pointsize = 18, res = 600)
print(p_bing + 
        p_text)  
dev.off()
