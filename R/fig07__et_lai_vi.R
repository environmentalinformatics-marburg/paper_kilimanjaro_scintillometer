### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
lib <- c("foreach", "dplyr", "plotrix", "grid", "gridBase")
Orcs::loadPkgs(lib)

## functions
source("R/slsAvlFls.R")
source("R/slsPlots.R")


### import data -----

## vi
dat_vi <- readRDS("data/modis_vi.rds")

## lai
dat_lai <- readRDS("data/modis_lai250.rds")
names(dat_lai)[5] <- "lai"

## merge vi and lai
dat_vi_lai <- merge(dat_vi, dat_lai, by = 1:4, sort = FALSE)

## et
fls_et <- slsAvlFls("/media/permanent/phd/scintillometer/data/sls/reprocess", 
                    ch_pattern = "mrg_rf_agg01h.csv$")

dat_et <- foreach(i = 1:nrow(fls_et), .combine = "rbind") %do% {
  dat <- read.csv(fls_et$mrg_rf_agg01h[i])
  data.frame(PlotID = fls_et$plot[i], habitat = fls_et$habitat[i], 
             season = fls_et$season[i], dat, 
             date = strftime(dat$datetime, "%Y-%m-%d"))
}

dat_et %>%
  group_by(PlotID, season, date) %>%
  summarise(waterET = sum(waterET, na.rm = FALSE)) %>%
  group_by(PlotID, season) %>%
  summarise(waterET.se = std.error(waterET, na.rm = TRUE), 
            waterET = mean(waterET, na.rm = TRUE)) -> dat_et

## merge lai and et
dat_mrg <- merge(dat_vi_lai, dat_et, by = c("PlotID", "season"), sort = FALSE)

id <- unlist(sapply(slsPlots("elevation"), function(i) grep(i, dat_mrg$PlotID)))
dat_mrg <- dat_mrg[id, ]

## models
mod_lai <- lm(waterET ~ log(lai), data = dat_mrg)
val_lai <- predict(mod_lai, data.frame(lai = seq(.05, 6, .01)))

mod_ndvi <- lm(waterET ~ ndvi, data = dat_mrg)
val_ndvi <- predict(mod_ndvi, data.frame(ndvi = seq(.3, .9, .01)))


### visualization -----

## point shapes
pts <- c(24, 25, 24, 25, 23, 23, 22, 22, 21, 21, 3, 13, 4)

## colors
clr <- c(rep("grey50", 2), rep("white", 2), "grey50", "white", "grey50", 
         "white", "grey50", "white", rep("black", 3))

## legend
plt <- slsPlots("elevation")
plt <- c(plt[1], "sav5 (d)", plt[2], "sav0 (d)", plt[3:length(plt)])

## write to file
ch_dir_pub <- "../../pub/papers/detsch_et_al__spotty_evapotranspiration/"
dir_ltx <- paste0(ch_dir_pub, "journals/ema/review/latex")
fls_out <- paste0(dir_ltx, "/img/Fig04.eps")

setEPS()
postscript(fls_out, width = 7, height = 6, fonts = c("serif", "Palatino"), pointsize = 13)

## legend
plot.new()
vp_top <- viewport(x = .5, y = .9, width = .9, height = .1)
pushViewport(vp_top)
par(new = TRUE, fig = gridFIG(), xpd = TRUE)
legend(x = -.05, y = .65, plt, pch = pts, pt.cex = 1.5, pt.bg = clr, pt.lwd = 1.25,
       ncol = 7, box.col = "transparent", x.intersp = 1.25, y.intersp = -4, 
       cex = .8)

## ndvi
upViewport()
vp_left <- viewport(x = 0, y = 0, width = 1, height = .95, 
                    just = c("left", "bottom"))
pushViewport(vp_left)
par(new = TRUE, fig = gridFIG(), xpd = TRUE)
plotCI(dat_mrg$ndvi, dat_mrg$waterET, dat_mrg$waterET.se, slty = 1, lwd = 1.2,
       scol = "grey50", pch = pts, cex = 1.75, xlab = "NDVI", ylab = "ET (mm/d)", 
       xlim = c(.3, .9), ylim = c(0, 7))
points(dat_mrg$ndvi, dat_mrg$waterET, pch = pts, bg = clr, cex = 1.75, lwd = 2)
lines(seq(.3, .9, .01), val_ndvi, lty = 2, lwd = 1.75, col = "red")
text(.85, .25, bquote("R"^"2" ~ "=" ~ .(round(summary(mod_ndvi)$r.squared, 2))))

dev.off()
