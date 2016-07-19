### environmental stuff -----

## packages
lib <- c("reset", "dplyr", "latticeExtra", "foreach")
Orcs::loadPkgs(lib)

## functions
source("R/slsFcts.R")

## data
load("data/licor_lai.rds")


### processing -----

fls <- slsAvlFls()$mrg_rf_agg01h
plt <- substr(basename(fls), 1, 4); luc <- substr(plt, 1, 3)

ssn <- slsAvlFls()$season

dat <- foreach(i = fls, j = plt, k = luc, .combine = "rbind") %do% {
  dat <- read.csv(i)
  dat <- dat[complete.cases(dat), ]
  dat$PlotID = j; dat$LUC <- k
  return(dat)
}                       

## linear regression, e.g. evapotranspiration vs. net radiation
mod0 <- lm(waterET ~ netRad + LUC, data = dat)
summary(mod0)

## backward elimination
dat1 <- dat[, c("tempUp", "humidity", 
                "soilHeatFlux", "pressure", "waterET", "vpd", "netRad", 
                "precipRate", "LUC")]

mod1 <- step(lm(waterET ~ ., data = dat1), direction = "backward")

## 
dat_agg <- foreach(i = fls, j = ssn, .combine = "rbind") %do% {
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
  
  data.frame(PlotID = substr(basename(i), 1, 4),
             dat_agg[, c("date", "waterET", "netRadMJ", "soilHeatFluxMJ", 
                         "delta", "gamma", "precipRate", "Epot")])
}

dat_agg$lai <- NA
dat_agg$lai[1:5] <- df_plt_lai$LAI[1]
dat_agg$lai[6:8] <- df_plt_lai$LAI[2]
dat_agg$lai[9:14] <- df_plt_lai$LAI[3]
dat_agg$lai[15:20] <- df_plt_lai$LAI[4]
dat_agg$lai[21:24] <- df_plt_lai$LAI[5]
dat_agg$lai[25:28] <- df_plt_lai$LAI[6]
dat_agg$lai[29:33] <- df_plt_lai$LAI[7]
dat_agg$lai[34:38] <- df_plt_lai$LAI[8]
dat_agg$lai[39:43] <- df_plt_lai$LAI[9]
dat_agg$lai[44:48] <- df_plt_lai$LAI[11]
dat_agg$lai[49:53] <- df_plt_lai$LAI[10]
dat_agg$lai[54:58] <- df_plt_lai$LAI[13]
dat_agg$lai[59:63] <- df_plt_lai$LAI[12]

xyplot(Epot ~ lai, data = dat_agg)
