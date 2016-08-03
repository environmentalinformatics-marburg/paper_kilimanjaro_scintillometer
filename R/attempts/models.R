fls <- slsAvlFls(ch_pattern = "mrg_rf_agg01h.csv$")

plots <- substr(basename(fls$mrg_rf_agg01h), 1, 4)

dat_srun <- foreach(i = fls$mrg_rf_agg01h, j = plots, .combine = "rbind") %do% {
  dat <- read.csv(i)
  dat$PlotID <- j
  return(dat)
}                       

dat_srun$vpd <- vpd(dat_srun$tempUp, dat_srun$humidity)
dat_srun$luc <- substr(dat_srun$PlotID, 1, 3)
dat_srun$luc <- as.factor(dat_srun$luc)
dat_srun$PlotID <- as.factor(dat_srun$PlotID)


### ET ~ downward radiation
library(lattice)
xyplot(waterET ~ dwnRad | luc, data = subset(dat_srun, dwnRad > 500))
xyplot(waterET ~ dwnRad | luc, data = dat_srun)


### lm -----

mod <- lm(waterET ~ dwnRad, data = dat_srun)
summary(mod)

mod1 <- lm(waterET ~ netRad, data = dat_srun)
summary(mod1)


### ctree -----

library(party)

mod2 <- ctree(residuals(mod) ~ luc +pressure + tempLw + upwRad + vpd, 
              data = subset(dat_srun, dwnRad > 500))
