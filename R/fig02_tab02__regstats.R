### environmental stuff -----

## clear working directory
rm(list = ls(all = TRUE))

## load prediction statistics
load("data/reg_stats_vpd.RData")

## source requiredfunctions
source("R/slsPlots.R")
source("R/slsPkgs.R")

## paths
dir_fig <- "../../pub/papers/detsch_et_al__spotty_evapotranspiration/fig/"


### tables -----

## short
dat_trn <- df_rf_scores_stats[, c("plot", "rmse_trn", "rmse_trns_se", "rsq_trn")]
dat_trn$plot <- as.character(dat_trn$plot)
dat_trn[12:13, "plot"] <- paste(dat_trn[12:13, "plot"], "(d)")

dat_trn <- dat_trn[rev(match(slsPlots("elevation", dry = TRUE), dat_trn$plot)), ]
dat_trn[, c(2, 4)] <- round(dat_trn[, c(2, 4)], 2)
dat_trn[, 3] <- round(dat_trn[, 3], 3)

stargazer(dat_trn, summary = FALSE, rownames = FALSE, digits = NA, 
          decimal.mark = ".")

## long
dat_trn_lng <- df_rf_scores_stats[, c("plot", "rmse_trn", "rmse_trns_se", 
                                      "rsq_trn", "MAE", "MAE.se", "RMSE", 
                                      "RMSE.se", "Rsq")]
dat_trn_lng$plot <- as.character(dat_trn_lng$plot)
dat_trn_lng[12:13, "plot"] <- paste(dat_trn_lng[12:13, "plot"], "(d)")

dat_trn_lng <- dat_trn_lng[rev(match(slsPlots("elevation", dry = TRUE), dat_trn_lng$plot)), ]
dat_trn_lng[, c(2, 4, 5, 7, 9)] <- round(dat_trn_lng[, c(2, 4, 5, 7, 9)], 2)
dat_trn_lng[, c(3, 6, 8)] <- round(dat_trn_lng[, c(3, 6, 8)], 3)

dat_trn_lng[, 2] <- paste(dat_trn_lng[, 2], dat_trn_lng[, 3], sep = " \u00b1 ")
dat_trn_lng <- dat_trn_lng[, -3]

dat_trn_lng[, 4] <- paste(dat_trn_lng[, 4], dat_trn_lng[, 5], sep = " \u00b1 ")
dat_trn_lng <- dat_trn_lng[, -5]

dat_trn_lng[, 5] <- paste(dat_trn_lng[, 5], dat_trn_lng[, 6], sep = " \u00b1 ")
dat_trn_lng <- dat_trn_lng[, -6]

stargazer(dat_trn_lng, summary = FALSE, rownames = FALSE, digits = NA, 
          decimal.mark = ".")

## ranges
for (i in c("rmse_trn", "rsq_trn"))
  cat(i, round(range(df_rf_scores_stats[, i]), 2), "\n")


### visualization -----

## ranges
for (i in c("ME", "MAE", "RMSE", "Rsq"))
  cat(i, round(range(df_rf_scores_stats[, i]), 2), "\n")

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
