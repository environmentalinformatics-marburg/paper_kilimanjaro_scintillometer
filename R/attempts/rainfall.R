### environmental stuff -----

## packages
lib <- c("reset", "dplyr", "latticeExtra", "foreach", "QuantPsyc")
Orcs::loadPkgs(lib)

## functions
source("R/slsFcts.R")


### processing -----

fls <- slsAvlFls()$mrg_rf_agg01h
ssn <- slsAvlFls()$season

dat_breaks <- foreach(i = fls, j = ssn, .combine = "rbind") %do% {
  
  dat <- read.csv(i)
  dat$datetime <- strptime(dat$datetime, format = "%Y-%m-%d %H:%M:%S")
  
  ## rain affected plots
  if (sum(dat$precipRate, na.rm = TRUE) > 0) {
    
    breaks <- c("1 hour", paste(c(2, 3, 4, 6, 12, 24), "hours"))
    lst_breaks <- lapply(breaks, function(k) {
      dat$datetime <- cut(dat$datetime, breaks = k)
      
      dat_agg <- dat %>% 
        group_by(datetime) %>%
        summarise(waterET = sum(waterET),
                  precipRate = sum(precipRate)) %>%
        # summarise(breaks = k, n = sum(complete.cases(waterET, precipRate)),
        #           r = cor(precipRate, waterET, use = "complete.obs")) 
        do(
          summarise(., breaks = k, n = sum(complete.cases(waterET, precipRate)), 
                    r = sqrt(summary(lm(waterET ~ precipRate, data = .))$r.squared), 
                    src = lm.beta(lm(waterET ~ precipRate, data = .)))
        )
      
      data.frame(PlotID = paste(substr(basename(i), 1, 4), j, sep = ", "),
                 dat_agg[, c("breaks", "n", "r")])
    })
    
    do.call("rbind", lst_breaks)
    
    ## rain free plots  
  } else {
    return(invisible())
  }
}
