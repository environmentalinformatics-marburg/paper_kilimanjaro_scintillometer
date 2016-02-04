### environmental stuff --------------------------------------------------------

## clear working directory
rm(list = ls(all = TRUE))

## load prediction statistics
load("data/reg_stats_vpd.RData")

## source requiredfunctions
source("R/slsPlots.R")
source("R/slsPkgs.R")

## paths
ch_dir_fig <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/fig/"


### visualization --------------------------------------------------------------

ch_sls_plt <- as.character(df_rf_scores_stats[, 1])

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
  update(ls_rf_scores_dryssn_vis[[i]], 
         main = list(paste0("\t", ch_sls_plt[i]), cex = .8, 
                     hjust = ifelse(i %in% c(1, 5, 9), .39, .65), vjust = 1.3))
})

## in-text png version
png(paste0(ch_dir_fig, "figure03.png"), width = 20, height = 12, 
    units = "cm", res = 500)
plot.new()
trellis.par.set("clip", list(panel = "off", strip = "off"))
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE, 
                                   widths = c(1.2, 1, 1, 1)), 
        ls_rf_scores_dryssn_vis)
dev.off()

## stand-alone tiff version
setEPS()
postscript(paste0(ch_dir_fig, "figure03.eps"), 
           width = 20*.3937, height = 12*.3937)
plot.new()
trellis.par.set("clip", list(panel = "off", strip = "off"))
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE, 
                                   widths = c(1.2, 1, 1, 1)), 
        ls_rf_scores_dryssn_vis)
dev.off()
