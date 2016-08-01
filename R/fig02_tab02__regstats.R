### environmental stuff --------------------------------------------------------

## clear working directory
rm(list = ls(all = TRUE))

## load prediction statistics
load("data/reg_stats_vpd.RData")

## source requiredfunctions
source("R/slsPlots.R")
source("R/slsPkgs.R")

## paths
dir_fig <- "../../pub/papers/detsch_et_al__spotty_evapotranspiration/fig/"


### visualization --------------------------------------------------------------

sls_plt <- as.character(df_rf_scores_stats[, 1])
sls_plt <- sls_plt[1:11]

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis1), function(i) {
  update(ls_rf_scores_dryssn_vis1[[i]], 
         main = list(paste0("\t", sls_plt[i]), cex = .8, 
                     hjust = ifelse(i %in% c(1, 5, 9), -.1, .35), vjust = 1.3))
})

ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[c(9, 10, 11, 11, 5:8, 1:4)]

## stand-alone eps version
setEPS()
postscript(paste0(dir_fig, "figure02.eps"), 
           width = 20*.3937, height = 12*.3937)
plot.new()
trellis.par.set("clip", list(panel = "off", strip = "off"))
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE, 
                                   widths = c(1.2, 1, 1, 1)), 
        ls_rf_scores_dryssn_vis)
grid.rect(x = .775, y = .7, width = .225, height = .3, just = c("left", "bottom"), 
          gp = gpar(col = "white", fill = "white"))
dev.off()
