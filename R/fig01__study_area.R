## environmental stuff

# packages
library(Rsenal)
library(plotrix)
library(latticeExtra)
library(gridExtra)

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
spl_kili <- rgb2spLayout(rst_kili, alpha = .8)

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
ch_loc_lbl <- sapply(int_loc_lbl, function(tmp_cnt) {
  if (tmp_cnt == 1) {
    return("top")
  } else if (tmp_cnt == 2) {
    return("right")
  } else if (tmp_cnt == 3) {
    return("bottom")
  } else {
    return("left")
  }
})

# relative label placement
num_rng_x <- num_xmax - num_xmin
num_crd_x_rel <- (mat_crd[, 1]-num_xmin) / num_rng_x

num_rng_y <- num_ymax - num_ymin
num_crd_y_rel <- (mat_crd[, 2]-num_ymin) / num_rng_y

mat_crd_rel <- matrix(c(num_crd_x_rel, num_crd_y_rel), ncol = 2)


# bing aerial including point locations
p_bing <- spplot(spp_plt_amp_sls, zcol = "PlotID", 
                 scales = list(draw = TRUE, cex = 1.25), 
                 col.regions = "white", cex = 1.75, pch = 20,
                 auto.key = FALSE, xlim = num_xlim, ylim = num_ylim, 
                 sp.layout = spl_kili) + 
  layer(sp.points(spp_plt_amp_sls, cex = 1.25, pch = 20, col = "black"))

# output filename
ch_fls_out <- paste0(ch_dir_ppr, "fig/fig01__study_area.png")

# figure
png(ch_fls_out, width = 30, height = 28, units = "cm", pointsize = 18, res = 300)

# bing image incl point locations
print(p_bing)

# insertion of shadow text
downViewport(trellis.vpname(name = "figure"))

for (tmp_cnt in 1:nrow(mat_crd_rel)) {
  x <- unit(mat_crd_rel[tmp_cnt, 1], "npc")
  y <- unit(mat_crd_rel[tmp_cnt, 2], "npc")
  
  ch_jst <- ch_loc_lbl[tmp_cnt]
  
  if (ch_jst %in% c("left", "right")) {
    if (ch_jst == "left") {x <- x+unit(.02, "npc")} else {x <- x-unit(.02, "npc")}
  } else {
    if (ch_jst == "top") {y <- y-unit(.02, "npc")} else {y <- y+unit(.02, "npc")}
  }
  
  grid.stext(spp_plt_amp_sls@data$PlotID[tmp_cnt], 
             x = x, y = y, 
             gp = gpar(fontsize = 25, fontfamily = "Bookman Old Style"), 
             just = ch_loc_lbl[tmp_cnt])
}

dev.off()
