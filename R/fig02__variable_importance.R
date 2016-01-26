### environmental stuff

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## path: output storage
ch_dir_out <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"


### processing

## variable importances
ch_fls_varimp <- list.files("data", pattern = "^varimp_final", 
                            full.names = TRUE)
ch_fls_varimp <- ch_fls_varimp[-grep("201409", ch_fls_varimp)]
ls_varimp <- lapply(ch_fls_varimp, read.csv)
df_varimp <- do.call("rbind", ls_varimp)
df_varimp <- df_varimp[, -1]

## refactorize method so that it is ordered according to performance statistics, 
## works only with dotplots results still in global environment
df_varimp_mlt <- melt(df_varimp, id.vars = c(1, 2))

var_count <- df_varimp_mlt %>%
  count(variable, plot)

var_stats <- df_varimp_mlt %>%
  group_by(plot, variable) %>%
  summarise(df_varimp_mlt = mean(value))

var_stats <- merge(var_stats, var_count)
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

var_stats[, 1] <- factor(var_stats[, 1], levels = slsPlots(style = "elevation"))

hmap <- levelplot(df_varimp_mlt ~ variable * plot, data = var_stats,
                  col.regions = clr(101), at = seq(0, 100, 1),
                  asp = 1, as.table = TRUE,
                  ylab = list(label = "Sampling plot", cex = .75), , xlab = "",
                  scales = list(x = list(cex = .7, 
                                         labels = c(expression("R"[dwn]), 
                                                    "p", "VPD", "S", "hr", 
                                                    expression("T"["a-20"]), 
                                                    expression("R"[up]), 
                                                    "rH", 
                                                    expression("T"["a-200"]))), 
                                y = list(cex = .7), tck = .6),
                  colorkey = FALSE,
                  panel=function(...) {
                    grid.rect(gp=gpar(col=NA, fill="grey60"))
                    panel.levelplot(...)
                  })

## in-text png version
png(paste0(ch_dir_out, "fig/figure03.png"), units = "cm", 
    width = 10, height = 12, res = 500)
print(hmap)

downViewport(trellis.vpname("figure"))
vp_key <- viewport(x = .5, y = 1.11)
pushViewport(vp_key)
draw.colorkey(key = list(col = clr(101), width = .6, height = .6, at = 0:100,
                         labels = list(at = seq(0, 100, 20), cex = .7), 
                         space = "top", tck = .6), draw = TRUE)
grid.text("Mean variable importance", x = .5, y = .6, 
          gp = gpar(cex = .8, fontface = "bold"))
dev.off()

## standalone eps version
setEPS()
postscript(paste0(ch_dir_out, "fig/figure03.eps"), width = 10*.3937, 
           height = 12*.3937)
print(hmap)

downViewport(trellis.vpname("figure"))
vp_key <- viewport(x = .5, y = 1.11)
pushViewport(vp_key)
draw.colorkey(key = list(col = clr(101), width = .6, height = .6, at = 0:100,
                         labels = list(at = seq(0, 100, 20), cex = .7), 
                         space = "top", tck = .6), draw = TRUE)
grid.text("Mean variable importance", x = .5, y = .6, 
          gp = gpar(cex = .8, fontface = "bold"))
dev.off()