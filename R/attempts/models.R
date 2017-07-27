### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")


### processing -----

## import and merge ndvi and lai
dat_vi <- readRDS("data/modis_vi.rds")

dat_lai <- readRDS("data/modis_lai250.rds")
names(dat_lai)[5] <- "lai"

dat_vi_lai <- merge(dat_vi, dat_lai, by = 1:4, sort = FALSE)

## import sls data
fls <- slsAvlFls("../../phd/scintillometer/data/sls/reprocess", ch_pattern = "mrg_rf_agg01h.csv$")

dat_srun <- foreach(i = fls$mrg_rf_agg01h, j = seq(nrow(fls)), .combine = "rbind") %do% {
  dat <- read.csv(i)
  dat$PlotID <- fls$plot[j]
  dat$season <- fls$season[j]
  return(dat)
}                       

dat_srun$PlotID <- as.factor(dat_srun$PlotID)

## aggregate 1-hour sls to daily values (maybe set `na.rm = FALSE`)
dat_srun$date <- substr(dat_srun$datetime, 1, 10)

dat_srun %>%
  group_by(PlotID, season, date) %>%
  summarise(netRad = mean(netRad, na.rm = FALSE), 
            pressure = mean(pressure, na.rm = FALSE), 
            soilHeatFlux = mean(soilHeatFlux, na.rm = FALSE), 
            vpd = mean(vpd, na.rm = FALSE), 
            precipRate = sum(precipRate, na.rm = FALSE),
            waterET = sum(waterET, na.rm = FALSE)) %>%
  data.frame() -> dat_dly

dat_dly <- dat_dly[complete.cases(dat_dly), ]

## aggregate 1-day sls data to one value (maybe set `na.rm = FALSE`)
dat_dly %>%
  group_by(PlotID, season) %>%
  summarise(netRad = mean(netRad, na.rm = FALSE), 
            pressure = mean(pressure, na.rm = FALSE), 
            soilHeatFlux = mean(soilHeatFlux, na.rm = FALSE), 
            vpd = mean(vpd, na.rm = FALSE), 
            precipRate = mean(precipRate, na.rm = FALSE),
            waterET = mean(waterET, na.rm = FALSE)) %>%
  data.frame() -> dat_one


### modeling, hourly values -----

## meteorological models

# per parameter, across all plots
for (i in c("netRad", "pressure", "soilHeatFlux", "vpd")) {
  mod <- lm(dat_srun[, "waterET"] ~ dat_srun[, i])
  cat(i, summary(mod)$r.squared, "\n")
}

# per parameter and for all parameters, per plot (table in manuscript)
dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ netRad, .)) %>% # net radiation
  summarise(PlotID = unique(PlotID), season = unique(season), 
            netRadRsq = summary(mod)$r.squared) -> rsq_rad

dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ pressure, .)) %>% # air pressure
  summarise(PlotID = unique(PlotID), season = unique(season), 
            pressureRsq = summary(mod)$r.squared) -> rsq_p

dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ soilHeatFlux, .)) %>% # soil heat flux
  summarise(PlotID = unique(PlotID), season = unique(season), 
            soilHeatFluxRsq = summary(mod)$r.squared) -> rsq_s

dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ vpd, .)) %>% # vpd
  summarise(PlotID = unique(PlotID), season = unique(season), 
            vpdRsq = summary(mod)$r.squared) -> rsq_vpd

dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ precipRate, .)) %>% # rainfall
  summarise(PlotID = unique(PlotID), season = unique(season), 
            precipRateRsq = summary(mod)$r.squared) -> rsq_rain

dat_srun %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + 
                precipRate, .)) %>%
  summarise(PlotID = unique(PlotID), season = unique(season), 
            Rsq = summary(mod)$r.squared) -> rsq_hrs

tab <- Reduce(function(...) merge(..., by = 1:2, sort = FALSE), 
              list(rsq_rad, rsq_p, rsq_s, rsq_vpd, rsq_rain, rsq_hrs))

tab$PlotID <- as.character(tab$PlotID)
tab$PlotID[c(10, 12)] <- paste(tab$PlotID[c(10, 12)], "(d)")
tab <- tab[rev(match(slsPlots("elevation", dry = TRUE), tab$PlotID)), ]
tab <- tab[, -2]

tab[, 2:ncol(tab)] <- round(tab[, 2:ncol(tab)], 2)

stargazer(tab, summary = FALSE, rownames = FALSE, digits = 2, 
          decimal.mark = ".")

## pooled meteorological data
mod_pld <- lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + precipRate, 
              data = dat_srun)
summary(mod_pld)$r.squared

## including ndvi 
dat_hrs <- merge(dat_srun, dat_vi_lai, by = c("PlotID", "season"))

dat_hrs %>%
  group_by(PlotID, season) %>%
  do(mod = lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + precipRate + ndvi, .)) %>%
  summarise(PlotID = unique(PlotID), season = unique(season), 
            Rsq = summary(mod)$r.squared) -> rsq_hrs_vi

## pooled meteorological data
mod_pld_vi <- lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + precipRate + ndvi, 
              data = dat_hrs)
summary(mod_pld_vi)$r.squared


### modeling, daily values -----

# ## per parameter, for daily values
# for (i in c("netRad", "pressure", "soilHeatFlux", "vpd")) {
#   mod <- lm(dat_agg[, "waterET"] ~ dat_agg[, i])
#   cat(i, summary(mod)$r.squared, "\n")
# }

dat_dly <- merge(dat_dly, dat_vi_lai, by = c("PlotID", "season"))

## without vegetation
mod_dly <- lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + precipRate, 
              data = dat_dly)
summary(mod_dly)$r.squared

## with vegetation
mod_dly_vi <- lm(waterET ~ netRad + pressure + soilHeatFlux + vpd + precipRate + 
                   ndvi + log(lai), data = dat_dly)
summary(mod_dly_vi)$r.squared


### modeling, one value -----

dat_one <- merge(dat_one, dat_vi_lai, by = c("PlotID", "season"))

## without vegetation
mod_one <- lm(waterET ~ netRad + pressure + soilHeatFlux + 
                vpd + precipRate, data = dat_one)
summary(mod_one)$r.squared

## with vegetation
mod_one_vi <- lm(waterET ~ netRad + pressure + soilHeatFlux + 
                   vpd + precipRate + ndvi, data = dat_one)
summary(mod_one_vi)$r.squared
