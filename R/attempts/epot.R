### environmental stuff -----

## packages
lib <- c("reset", "dplyr", "latticeExtra", "foreach")
Orcs::loadPkgs(lib)

## functions
source("R/slsFcts.R")


### processing -----

dsn <- switch(Sys.info()[["sysname"]], 
              "Windows" = "E:/phd/scintillometer/data", 
              "Linux" = "/media/permanent/phd/scintillometer/data")

fls <- slsAvlFls(dsn)$mrg_rf_agg01h
ssn <- slsAvlFls(dsn)$season

dat_epot <- foreach(i = fls, j = ssn, .combine = "rbind") %do% {
  dat <- read.csv(i)
  dat$datetime <- strptime(dat$datetime, format = "%Y-%m-%d %H:%M:%S")
  dat$date <- as.Date(dat$datetime); dat <- dat[, -1]
  
  dat_agg <- dat %>% 
    group_by(date) %>%
    mutate(netRadMJ = reset:::toJoules(netRad), 
           soilHeatFluxMJ = reset:::toJoules(soilHeatFlux), 
           delta = vaporPressureSat(tempUp, slope = TRUE), 
           gamma = psychrometricConstant(pressure * .1)) %>%
    summarise(waterET = sum(waterET),
              netRadMJ = sum(netRadMJ),
              soilHeatFluxMJ = sum(soilHeatFluxMJ),
              delta = mean(delta),
              gamma = mean(gamma),
              precipRate = sum(precipRate)) %>%
    mutate(Epot = potentialEvaporation(netRadMJ, soilHeatFluxMJ, delta, gamma)) 
  
  data.frame(PlotID = paste(substr(basename(i), 1, 4), j, sep = ", "),
             dat_agg[, c("waterET", "netRadMJ", "soilHeatFluxMJ", 
                         "delta", "gamma", "precipRate", "Epot")])
}

xyplot(Epot ~ waterET | PlotID, data = dat_epot, panel = function(x, y, ...) {
  panel.xyplot(x, y, col = "grey50")
  panel.ablineq(lm(y ~ x), rotate = TRUE, r.squared = TRUE, at = .8)
})


### -----

dat_epot %>% 
  group_by(PlotID) %>%
  summarise(waterET.se = round(plotrix::std.error(waterET, na.rm = TRUE), 2), 
            Epot.se = round(plotrix::std.error(Epot, na.rm = TRUE), 2), 
            waterET = round(mean(waterET, na.rm = TRUE), 2), 
            Epot = round(mean(Epot, na.rm = TRUE), 2)) %>%
  mutate(Difference = Epot - waterET) %>%
  data.frame() -> potET

potET$Season <- sapply(strsplit(as.character(potET$PlotID), ", "), "[[", 2)
potET$PlotID <- sapply(strsplit(as.character(potET$PlotID), ", "), "[[", 1)

potET$waterET <- paste(potET$waterET, potET$waterET.se, sep = " +/- ")
potET$Epot <- paste(potET$Epot, potET$Epot.se, sep = " +/- ")
potET <- potET[, c(1, 7, 4, 5, 6)]

ids <- unlist(
  sapply(rev(slsPlots("elevation")), function(i) grep(i, potET$PlotID))
)
potET <- potET[ids, ]

library(stargazer)
stargazer(potET, summary = FALSE, rownames = FALSE)
