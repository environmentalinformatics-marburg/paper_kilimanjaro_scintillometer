### environmental stuff

## clear working directory
rm(list = ls(all = TRUE))

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## path: output storage
dir_out <- "../../pub/papers/detsch_et_al__spotty_evapotranspiration/"


### processing

## variable importances
# fls_varimp <- list.files("data", pattern = "^varimp_final",
#                             full.names = TRUE)
# fls_varimp <- fls_varimp[-grep("201409", fls_varimp)]
# fls_varimp <- list.files("data", pattern = "^varimp_vpd", full.names = TRUE)
# fls_varimp <- fls_varimp[-grep("agg10m", fls_varimp)]
# fls_avl <- slsAvlFls()[, 1:3]
# df_varimp <- foreach(i = fls_varimp, j = 1:length(fls_varimp), 
#                      .combine = "rbind") %do% {
#   dat <- read.csv(i)
#   dat$season <- fls_avl$season[j]
#   return(dat)
# }
# df_varimp <- df_varimp[, -1]

load("data/reg_stats_vpd.RData")
df_rf_scores_dryssn_varimp$plot <- as.character(df_rf_scores_dryssn_varimp$plot)
df_rf_scores_dryssn_varimp$plot[12] <- paste(df_rf_scores_dryssn_varimp$plot[12], "(d)")
df_rf_scores_dryssn_varimp$plot[13] <- paste(df_rf_scores_dryssn_varimp$plot[13], "(d)")
df_varimp <- df_rf_scores_dryssn_varimp

## refactorize method so that it is ordered according to performance statistics, 
## works only with dotplots results still in global environment
# df_varimp_mlt <- melt(df_varimp, id.vars = c("plot", "mtry", "season"))
df_varimp_mlt <- melt(df_varimp, id.vars = c("plot", "mtry"))

var_count <- df_varimp_mlt %>%
  # group_by(season) %>%
  count(variable, plot)

var_stats <- df_varimp_mlt %>%
  # group_by(plot, season, variable) %>%
  group_by(plot, variable) %>%
  summarise(df_varimp_mlt = mean(value))

# var_stats <- merge(var_stats, var_count, by = c("plot", "season", "variable"))
var_stats <- merge(var_stats, var_count, by = c("plot", "variable"))
var_stats$varimp_weighted <- var_stats$df_varimp_mlt * var_stats$n / 10

var_stats_ovrall <- var_stats %>%
  group_by(variable) %>%
  summarise(rank = mean(varimp_weighted))

var_stats_ovrall <- var_stats_ovrall[order(var_stats_ovrall$rank,
                                           decreasing = TRUE), ]
var_stats_ovrall$variable <- as.character(var_stats_ovrall$variable)

var_stats$variable <- factor(var_stats$variable,
                             levels = var_stats_ovrall$variable)

clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))

# var_stats$plot_season <- as.character(var_stats$plot)
# var_stats[var_stats$season == "d", "plot_season"] <- 
#   paste(var_stats[var_stats$season == "d", "plot_season"], "(d)")
# levels <- slsPlots(style = "elevation")
# levels <- c(levels[1], "sav5 (d)", levels[2], "sav0 (d)", levels[3:length(levels)])
# var_stats$plot_season <- factor(var_stats$plot_season, levels = levels)
var_stats$plot <- factor(var_stats$plot, levels = slsPlots("elevation", dry = TRUE))

hmap <- levelplot(df_varimp_mlt ~ variable * plot, data = var_stats,
                  col.regions = clr(101), at = seq(0, 100, 1),
                  asp = 1, as.table = TRUE,
                  ylab = list(label = "Plot ID", cex = .75), xlab = "",
                  scales = list(x = list(cex = .7, 
                                         labels = c(expression("R"[net]), 
                                                    "p", "S", "VPD",  
                                                    expression("T"["a"]), 
                                                    "rH", "Rain")), 
                                y = list(cex = .7), tck = .6),
                  colorkey = FALSE,
                  panel=function(...) {
                    grid.rect(gp=gpar(col=NA, fill="grey60"))
                    panel.levelplot(...)
                  })

## standalone eps version
setEPS()
postscript(paste0(dir_out, "fig/figure03.eps"), width = 10*.3937, 
           height = 14*.3937)
print(hmap)

downViewport(trellis.vpname("figure"))
vp_key <- viewport(x = .5, y = 1.11)
pushViewport(vp_key)
draw.colorkey(key = list(col = clr(101), width = .6, height = .6, at = 0:100,
                         labels = list(at = seq(0, 100, 20), cex = .7), 
                         space = "top", tck = .6), draw = TRUE)
grid.text("Mean variable importance", x = .5, y = .6, 
          gp = gpar(cex = .7, fontface = "bold"))
dev.off()
