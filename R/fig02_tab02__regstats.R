## environmental stuff

rm(list = ls(all = TRUE))
load("data/reg_stats_whr.RData")

# functions
source("R/slsPlots.R")

# path: output
ch_dir_fig <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/fig/"


## visualization

ch_sls_plt <- slsPlots()

fc_rf_plt <- df_rf_scores_stats$plot
ch_rf_plt <- as.character(fc_rf_plt)

int_id_plt <- match(ch_rf_plt, ch_sls_plt)
ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[int_id_plt]

# add plot names to figures
ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
  update(ls_rf_scores_dryssn_vis[[i]], main = paste0("\t", ch_sls_plt[i]))
})

png(paste0(ch_dir_fig, "fig02__reg_stats.png"), width = 35, height = 20, 
    units = "cm", pointsize = 15, res = 300)
do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE), 
        ls_rf_scores_dryssn_vis)
dev.off()


###########
latticeCombineGrid(ls_rf_scores_dryssn_vis, x.same = TRUE, y.same = TRUE)
