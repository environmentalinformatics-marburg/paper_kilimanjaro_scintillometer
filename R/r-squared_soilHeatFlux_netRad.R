source("R/slsPkgs.R")
source("R/slsFcts.R")

fls <- slsAvlFls("../../phd/scintillometer/data/sls/reprocess")
plt <- substr(basename(fls$plot))
r <- do.call("rbind", lapply(fls$mrg_rf_agg01h, function(i) {
  dat <- read.csv(i)
  data.frame(precip = any(dat$precipRate > 0, na.rm = TRUE), 
             dwnRad = cor(dat$soilHeatFlux, dat$dwnRad, use = "complete.obs")^2, 
             dratio = mean(dat$soilHeatFlux / dat$dwnRad, na.rm = TRUE), 
             upwRad = cor(dat$soilHeatFlux, dat$upwRad, use = "complete.obs")^2, 
             uratio = mean(dat$soilHeatFlux / dat$upwRad, na.rm = TRUE), 
             netRad = cor(dat$soilHeatFlux, dat$netRad, use = "complete.obs")^2, 
             nratio = mean(dat$soilHeatFlux / dat$netRad, na.rm = TRUE))
}))

r$PlotID <- fls$plot
r$Season <- fls$season

ids <- do.call("c", lapply(rev(slsPlots("elevation")), function(i) {
  grep(i, r$PlotID)
}))
r[ids, ]


### raw (1-min) basis -----

lst <- foreach(i = 1:nrow(fls), .combine = "rbind") %do% {
  dat <- read.csv(fls$mrg_rf_agg01h[i])
  data.frame(PlotID = fls$plot[i], habitat = fls$habitat[i], 
             season = fls$season[i], dat, 
             date = strftime(dat$datetime, "%Y-%m-%d"))
}

lst %>%
  group_by(PlotID, season) %>%
  summarise(nratio = mean(soilHeatFlux / netRad, na.rm = TRUE), 
            netRad = cor(soilHeatFlux, netRad, use = "complete.obs")^2) %>%
  data.frame() -> raw


### daily basis -----

lst <- foreach(i = 1:nrow(fls), .combine = "rbind") %do% {
  dat <- read.csv(fls$mrg_rf_agg01h[i])
  data.frame(PlotID = fls$plot[i], habitat = fls$habitat[i], 
             season = fls$season[i], dat, 
             date = strftime(dat$datetime, "%Y-%m-%d"))
}

lst %>%
  group_by(PlotID, season, date) %>%
  summarise(soilHeatFlux = mean(soilHeatFlux, na.rm = TRUE), 
            netRad = mean(netRad, na.rm = TRUE), 
            nratio = soilHeatFlux / netRad) %>%
  group_by(PlotID, season) %>%
  summarise(nratio_mu = mean(nratio, na.rm = TRUE), 
            nratio_mx = max(nratio, na.rm = TRUE),
            netRad = cor(soilHeatFlux, netRad, use = "complete.obs")^2) %>%
  data.frame() -> dly

## with best fit lags
lst %>%
  group_by(PlotID, season) %>%
  summarise(lag = which.max(as.numeric(ccf(soilHeatFlux, netRad, lag.max = 6, 
                                na.action = na.pass, plot = FALSE)[[1]])),
            rsq = max(as.numeric(ccf(soilHeatFlux, netRad, lag.max = 6, 
                                   na.action = na.pass, plot = FALSE)[[1]]))^2) %>%
  data.frame() -> lags

slt <- split(lst, as.factor(paste(lst$PlotID, lst$season)))
lags$ratio <- sapply(1:length(slt), function(i) {
  if (lags$lag[i] == 7) {
    mean(slt[[i]]$soilHeatFlux / slt[[i]]$netRad, na.rm = TRUE)
  } else {
    x <- c(NA, slt[[i]]$soilHeatFlux)
    y <- c(slt[[i]]$netRad, NA)
    mean(x / y, na.rm = TRUE)
  }
})
