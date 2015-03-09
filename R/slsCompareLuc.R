# Environmental stuff
rm(list = ls(all = TRUE))

lib <- c("randomForest", "matrixStats", "foreach", "ggplot2", "latticeExtra")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

srunWorkspaces <- dir("SRun/", pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

fls <- list.files(pattern = "_mrg_rf_agg01h.csv", recursive = TRUE, 
                  full.names = TRUE)

dat <- do.call("rbind", lapply(fls, function(i) {
  plt <- sapply(strsplit(basename(i), "_"), "[[", 1)
  dat <- read.csv(i)
  return(data.frame(plotid = plt, dat))
}))
dat$datetime <- strptime(dat$datetime, format = "%Y-%m-%d %H:%M")

png("phd/scintillometer/out/slsCompareLuc.png", width = 25, height = 30, 
    units = "cm", res = 300, pointsize = 18)
ggplot(aes(x = datetime, y = waterET), data = dat) + 
  geom_histogram(stat = "identity") + 
  facet_wrap(~ plotid, ncol = 2, scales = "free_x") + 
  geom_hline(aes(y = 0), colour = "darkgrey") + 
  labs(x = "Time [h]", y = "Evapotranspiration [mm/h]") + 
  theme_bw()
dev.off()

# Compare boxplot time series of corresponding natural and disturbed LUCs
fls <- list.files(pattern = "mrg.csv", recursive = TRUE, full.names = TRUE)
plt <- c("sav0", "mai0", "sav5", "mai4", "gra1", "cof3", "gra2", "cof2")

dat <- do.call("rbind", lapply(plt, function(i) {
  # Import current LUC
  tmp.fls <- fls[grep(i, fls)]
  tmp.dat <- read.csv(tmp.fls)[, c("datetime", "waterET")]
  
  # Split by day
  tmp.dat$time <- substr(tmp.dat$datetime, 12, 19)
  tmp.dat.split <- split(tmp.dat, substr(tmp.dat$datetime, 1, 10))
  
  # Merge by minute of day
  tmp.dat.mrg <- Reduce(function(...) merge(..., by = "time"), tmp.dat.split)
  tmp.dat.mrg <- tmp.dat.mrg[, -grep("datetime", names(tmp.dat.mrg))]
  
  # Return rowMedians per minute of day
  tmp.mat.mrg <- as.matrix(tmp.dat.mrg[, 2:ncol(tmp.dat.mrg)])
  return(data.frame(plotid = i, 
                    time = tmp.dat.mrg$time,
                    #  waterET = round(rowMeans(tmp.mat.mrg, na.rm = TRUE), 2)
                    waterET = round(rowMedians(tmp.mat.mrg, na.rm = TRUE), 2)
                    #  waterET = apply(tmp.mat.mrg, 1, FUN = function(...) modal(..., na.rm = TRUE))
  ))
}))

dat$hour <- as.factor(substr(dat$time, 1, 2))

dat$facet <- "SAV0 vs. MAI0"
dat$facet[dat$plotid %in% c("sav5", "mai4")] <- "SAV5 vs. MAI4"
dat$facet[dat$plotid %in% c("gra1", "cof3")] <- "GRA1 vs. COF3"
dat$facet[dat$plotid %in% c("gra2", "cof2")] <- "GRA2 vs. COF2"
dat$facet <- factor(dat$facet, levels = c("SAV0 vs. MAI0", "SAV5 vs. MAI4", 
                                          "GRA1 vs. COF3", "GRA2 vs. COF2"))

dat$luc <- factor(substr(dat$plotid, 1, 3), levels = c("sav", "mai", "gra", "cof"))

ggplot(aes(x = hour, y = waterET, fill = luc), data = dat) + 
  geom_boxplot(outlier.colour = NA, notch = TRUE) + 
  facet_wrap(~ facet) +
  scale_fill_manual("Land-cover type", 
                    values = c("darkgoldenrod", "darkolivegreen", 
                               "chartreuse", "burlywood4")) +
  ylim(-.05, 1) +
  labs(x = "Hour of day", y = "Evapotranspiration [mm/h]") + 
  theme_bw()

# Same story, but single plots
cols <- c("sav0" = "darkgoldenrod", "mai0" = "darkolivegreen", 
          "sav5" = "darkgoldenrod", "mai4" = "darkolivegreen", 
          "gra1" = "chartreuse", "cof3" = "burlywood4", 
          "gra2" = "chartreuse", "cof2" = "burlywood4")

foreach(h = list(c("sav0", "mai0"), c("sav5", "mai4"), 
                 c("gra1", "cof3"), c("gra2", "cof2"))) %do% {
                   
                   dat <- do.call("rbind", lapply(h, function(i) {
                     # Import current LUC
                     tmp.fls <- fls[grep(i, fls)]
                     tmp.dat <- read.csv(tmp.fls)[, c("datetime", "waterET")]
                     
                     # Split by day
                     tmp.dat$time <- substr(tmp.dat$datetime, 12, 19)
                     tmp.dat.split <- split(tmp.dat, substr(tmp.dat$datetime, 1, 10))
                     
                     # Merge by minute of day
                     tmp.dat.mrg <- Reduce(function(...) merge(..., by = "time"), tmp.dat.split)
                     tmp.dat.mrg <- tmp.dat.mrg[, -grep("datetime", names(tmp.dat.mrg))]
                     
                     # Return rowMedians per minute of day
                     tmp.mat.mrg <- as.matrix(tmp.dat.mrg[, 2:ncol(tmp.dat.mrg)])
                     return(data.frame(plotid = i, 
                                       time = tmp.dat.mrg$time,
                                       waterET = round(rowMedians(tmp.mat.mrg, na.rm = TRUE), 2)
                                       #                       waterET = apply(tmp.mat.mrg, 1, FUN = function(...) modal(..., na.rm = TRUE))
                     ))
                   }))
                   
                   dat$hour <- as.factor(substr(dat$time, 1, 2))
                   
                   ggplot(aes(x = hour, y = waterET, fill = plotid), data = dat) + 
                     geom_boxplot(outlier.colour = NA, notch = TRUE) + 
                     scale_fill_manual("PlotID", values = cols) +
                     ylim(-.05, 1) +
                     labs(x = "Hour of day", y = "Evapotranspiration [mm/h]") + 
                     theme_bw()
}