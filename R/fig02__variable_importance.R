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

var_stats[, 1] <- factor(var_stats[, 1], levels = slsPlots())

hmap <- levelplot(df_varimp_mlt ~ variable * plot, data = var_stats,
                  col.regions = clr(101), at = seq(0, 100, 1),
                  asp = 1, as.table = TRUE,
                  ylab = list(label = "Research site", cex = 1.5), , xlab = "",
                  scales = list(x = list(rot = 45, cex = 1.25), 
                                y = list(cex = 1.25)),
                  main = list(label = "Mean variable importance", cex = 1.6),
                  colorkey = list(space = "top",
                                  width = 1, height = 0.75),
                  panel=function(...) {
                    grid.rect(gp=gpar(col=NA, fill="grey60"))
                    panel.levelplot(...)
                  })

png(paste0(ch_dir_out, "fig/fig02__varimp.png"), units = "cm", 
    width = 20, height = 25, pointsize = 21, res = 300)
print(hmap)
dev.off()
