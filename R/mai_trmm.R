## required packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## avl mai4 data
df_fls <- slsAvlFls(ch_pattern = "mrg_rf_agg10m.csv$")
df_fls_mai <- subset(df_fls, plot == "mai4")

ls_dat_mai <- lapply(1:nrow(df_fls_mai), function(i) {
  tmp_df_dat <- read.csv(df_fls_mai[i, "mrg_rf_agg10m"])
  data.frame(PlotID = df_fls_mai[i, "plot"], tmp_df_dat)
})
df_dat_mai <- do.call("rbind", ls_dat_mai)
df_dat_mai$datetime <- as.POSIXct(df_dat_mai$datetime, format = "%Y-%m-%d %H:%M:%S")

## download trmm binary files and return filenames
df_fls_trmm <- downloadTRMM(begin = df_dat_mai$datetime[1], 
                            end = df_dat_mai$datetime[nrow(df_dat_mai)], 
                            dsn = "../../phd/scintillometer/data/trmm/")

## add date column
ch_dt_trmm <- substr(basename(df_fls_trmm$bin), 12, 21)
dt_trmm <- as.Date(ch_dt_trmm, format = "%Y.%m.%d")
yday(dt_trmm) <- yday(dt_trmm) - 1
df_fls_trmm$date <- dt_trmm

## rasterize trmm binary files
ls_rst_trmm <- lapply(1:nrow(df_fls_trmm), function(i) {
  rst_trmm <- rasterizeTRMM(binary = df_fls_trmm[i, 1], meta = df_fls_trmm[i, 2])
})
rst_trmm <- stack(ls_rst_trmm)

## cut to kili extent
rst_kili <- kiliAerial(rasterize = TRUE)
ext_kili_ll <- projectExtent(rst_kili, projection(rst_trmm))
rst_trmm_crp <- crop(rst_trmm, ext_kili_ll, snap = "near")
rst_trmm_crp_utm <- projectRaster(rst_trmm_crp, crs = projection(rst_kili))

## labelled contours
dem <- raster("../../kilimanjaro/coordinates/coords/DEM_ARC1960_30m_Hemp.tif")
dem_flipped <- flip(dem, "y")
x <- coordinates(dem_flipped)[, 1]
y <- coordinates(dem_flipped)[, 2]
z <- dem_flipped[]

p_dem <- levelplot(z ~ x * y, colorkey = FALSE, at = seq(1000, 6000, 1000), 
                   panel = function(...) {
                     panel.smoothconts(zlevs.conts = seq(1000, 5500, 500), 
                                       labels = c(1000, "", 2000, "", 3000, "", 4000, "", 5000, ""), 
                                       col = "grey50", labcex = .3, ...)
                   })

## point coordinates
ch_dir_crd <- "../../kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"
spy_plot <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd)
spy_mai4 <- subset(spy_plot, PoleType == "AMP" & PlotID == "mai4")

## visualize trmm rainfall
blues <- colorRampPalette(brewer.pal(9, "Blues"))
breaks <- seq(0, ceiling(max(maxValue(rst_trmm_crp_utm))), 1)

ls_p <- lapply(1:nlayers(rst_trmm_crp_utm), function(i) {
  levelplot(rst_trmm_crp_utm[[i]], col.regions = blues(length(breaks)), 
            margin = FALSE, scales = list(draw = TRUE), at = breaks,
            xlab = list(label = "x", cex = 1.1), ylab = list(label = "y", cex = 1.1)) + 
    as.layer(p_dem) + 
    layer(sp.points(spy_mai4, col = "black", cex = 1.2)) + 
  layer(sp.text(c(335000, 9687500), txt = df_fls_trmm$date[i], cex = .9))
})

png("../../phd/scintillometer/out/trmm_mai4.png", width = 15, height = 25, 
    units = "cm", pointsize = 15, res = 300)
plot.new()
print(latticeCombineGrid(ls_p), layout = c(3, 2))
dev.off()
