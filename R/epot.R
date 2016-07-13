### environmental stuff -----

## packages
lib <- c("reset", "dplyr", "latticeExtra", "foreach")
Orcs::loadPkgs(lib)

## functions
source("R/slsFcts.R")


### processing -----

fls <- slsAvlFls()$mrg_rf_agg01h
ssn <- slsAvlFls()$season

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
    # summarise(waterET = sum(waterET),
    #           netRadMJ = sum(netRadMJ),
    #           soilHeatFluxMJ = sum(soilHeatFluxMJ),
    #           delta = mean(delta),
    #           gamma = mean(gamma),
    #           precipRate = sum(precipRate)) %>%
    mutate(Epot = potentialEvaporation(netRadMJ, soilHeatFluxMJ, delta, gamma)) 
  
  data.frame(PlotID = paste(substr(basename(i), 1, 4), j, sep = ", "),
             dat_agg[, c("waterET", "netRadMJ", "soilHeatFluxMJ", 
                         "delta", "gamma", "precipRate", "Epot")])
}

xyplot(Epot ~ waterET | PlotID, data = dat_epot, panel = function(x, y, ...) {
  panel.xyplot(x, y, col = "grey50")
  panel.ablineq(lm(y ~ x), rotate = TRUE, r.squared = TRUE, at = .8)
})
