fls_srun <- list.files("data/SRun", recursive = TRUE, pattern = "mrg_rf_agg01h", 
                       full.names = TRUE)

plots <- substr(basename(fls_srun), 1, 4)

dat_srun <- foreach(i = fls_srun, j = plots, .combine = "rbind") %do% {
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


### lm -----

mod <- lm(waterET ~ dwnRad, data = subset(dat_srun, dwnRad > 500))
summary(mod)

# mod0 <- lm(waterET ~ dwnRad * as.factor(luc), data = subset(dat_srun, dwnRad > 500))
# summary(mod0)


### ctree -----

library(party)

mod2 <- ctree(residuals(mod) ~ luc +pressure + tempLw + upwRad + vpd, 
              data = subset(dat_srun, dwnRad > 500))
