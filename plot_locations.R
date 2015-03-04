# Required packages
library(OpenStreetMap)
library(TeachingDemos)

switch(Sys.info()[["sysname"]], 
       "Linux" = setwd("/mnt/windows/Permanent/kilimanjaro/coordinates/coords"))

# OSM data
kili <- openmap(upperLeft = c(-2.9, 37.15), lowerRight = c(-3.4, 37.75), 
                type = "bing", minNumTiles = 40)

# Plot data
plots <- readOGR(getwd(), "PlotPoles_ARC1960_mod_20140424_final")
plots.df <- plots@data
plots.amp <- subset(plots, PoleType == "AMP")
plots.amp.sub <- 
  plots.amp[plots.amp@data$PlotID %in% c("sav5", "sav4", "mai4", "mai1", 
                             "cof3", "cof2", "gra1", "gra2", 
                             "hel1", "fer0", "fed2"), ]
plots.amp.sub.mrc <- spTransform(plots.amp.sub, kili$tiles[[1]][[3]])

# plots <- read.csv("/media/permanent/kilimanjaro/plot_landuse/station_master.csv")[, 1:8]
# plots <- plots[complete.cases(plots), ]
# plots <- plots[!substr(plots$PlotID, 1, 3) %in% c("jul", "sun", "nkw", "kid", 
#                                                   "foc", "fod"), ]
# coordinates(plots) <- ~ Lon + Lat
# projection(plots) <- CRS("+init=epsg:4326")
# plots <- spTransform(plots, kili$tiles[[1]][[3]])
# 
# # Focal plots and corresponding research plots
# plots.foc <- subset(plots, FocalPlot == "Y" &
#                       !PlotID %in% c("flm1", "fpo1", "fpd2"))
# # Optional plots along the upper and lower forest boundary
# plots.opt <- subset(plots, PlotID %in% c("flm1", "fpo1", "fpd2", "hom1"))

# Plotting
png("/mnt/windows/Permanent/phd/summerschool_swiss_aug2014/figures/plot_locations.png", 
    width = 30, height = 28, units = "cm", pointsize = 15, res = 300)
par(family = "mono")

plot(kili)
points(plots.amp.sub.mrc, col = "orange", pch = 20)
text(plots.amp.sub.mrc, labels = plots.amp.sub.mrc$PlotID, adj = c(1.2, .4), col = "orange", 
     family = "mono", font = 2, cex = 1.5)

# shadowtext(x = coordinates(plots.amp.sub.mrc)[, 1], 
#            y = coordinates(plots.amp.sub.mrc)[, 2], 
#            labels = plots.amp.sub.mrc$PlotID, cex = 1.2, font = 1, 
#            col = "black", bg = "white", pos = c(4, 1))

# points(plots.opt, col = "steelblue", pch = 20)
# text(plots.opt, labels = plots.opt$PlotID, pos = 4, col = "steelblue", 
#      family = "mono", font = 2, cex = 1.2)
# legend(x = 4183700, y = -323000, legend = c("Focal plot", "Potential boundary plot"), 
#        pch = 20, col = c("orange", "steelblue"), text.col = c("orange", "steelblue"), 
#        text.font = 2)
# legend(x = 4183700, y = -323000, legend = c("Focal plot", "Potential boundary plot"), 
#        pch = 20, col = c("orange", "steelblue"), text.col = c("orange", "steelblue"), 
#        text.font = 2)
dev.off()
