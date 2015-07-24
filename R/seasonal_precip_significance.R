rm(list=ls(all=TRUE))

library(season)
library(quantreg)
library(ggplot2)

## parallelization
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)

# prep$P_SQRT2 <- prep$P_MEAN_NEW**0.25
# prep$mnth <- as.numeric(as.character(prep$mnth))
# prep$mnth_precent <- prep$mnth / 12
# prep$ann <- as.numeric(as.character(prep$ann))
# prep$date_precent <- prep$ann + prep$mnth_precent

df_all$ETmu_SQRT2 <- df_all$ETmu**0.25
df_all$Time_percent <- (as.integer(substr(df_all$Time, 1, 2)) + 1) / 24

sin_month_percent <- sin(2 * pi * df_all$Time_percent)
cos_month_percent <- cos(2 * pi * df_all$Time_percent)

### GLM
# significant, aber nicht normalverteilt
# season as.factor() testet alle Dekaden in einem Modell
mod_glm <- glm(df_all$ETmu ~ 
                 sin_month_percent + cos_month_percent + df_all$PlotID)
summary(mod_glm)

# normalverteilt, aber keine significance
mod_glm_sqrt2 <- glm(df_all$ETmu_SQRT2 ~ 
                       sin_month_percent + cos_month_percent + df_all$PlotID)
summary(mod_glm_sqrt2)

# Bootstrap (z. B. f|r El Nino Zeugsel)
# Bootstrap (1000 oder 2000 mal zufaellig verteilen)
# jeweils lm und estimate des in Frage kommenden Faktors
# Vergleich des tatsdchlichen estimates mit 95% (einseitig) bzw. 
# 97.5 (zweiseitig) des randominiserten
summary(mod_glm_sqrt2, se = "nid")

## bootstrapping
mod_bs <- foreach(x = seq(2000), 
                  .combine = function(...) data.frame(rbind(...))) %dopar% {
  set.seed(x)
  boot <- sample(nrow(df_all), replace = FALSE) 
  plotid <- as.factor(df_all$PlotID[boot])
  act_mod <- lm(df_all$ETmu_SQRT2 ~ 
                  sin_month_percent + cos_month_percent + plotid)
  return(act_mod$coefficients)
}

quantile(mod_bs$plotidcof3, probs = 0.975, na.rm = TRUE)
quantile(mod_bs$plotidcof3, probs = 0.95, na.rm = TRUE)
quantile(mod_bs$plotidcof3, probs = 0.05, na.rm = TRUE)
quantile(mod_bs$plotidcof3, probs = 0.025, na.rm = TRUE)
sort(mod_bs$plotidcof3)

## plot pairs
lapply(ls_all, function(i) {
  lapply(ls_all, function(j) {
    df_sub <- do.call("rbind", list(i, j))
    
    # ccf
    ccf(i$ETmu, j$ETmu)
    
    df_sub$ETmu_SQRT2 <- df_sub$ETmu**0.25
    df_sub$Time_percent <- (as.integer(substr(df_sub$Time, 1, 2)) + 1) / 24
    
    sin_month_percent <- sin(2 * pi * df_sub$Time_percent)
    cos_month_percent <- cos(2 * pi * df_sub$Time_percent)
    
    mod_bs <- lapply(seq(2000), function(x) {
      # Jetzt sampeln, dann rechnen
      set.seed <- x
      boot <- sample(nrow(df_sub), replace = FALSE) 
      plotid <- as.factor(df_sub$PlotID[boot])
      act_mod <- lm(df_sub$ETmu_SQRT2 ~ 
                      sin_month_percent + cos_month_percent + plotid)
      return(act_mod$coefficients)
    })
    mod_bs <- do.call("rbind", mod_bs)
    
    quantile(mod_bs[, 4], probs = 0.01)
  })
})

quantile(mod_bs$season1983, probs = 0.975)
quantile(mod_bs$season1983, probs = 0.95)
quantile(mod_bs$season1983, probs = 0.05)
quantile(mod_bs$season1983, probs = 0.025)
mod_lm_sqrt2_org
sort(mod_bs$season1983)

quantile(mod_bs$season1993, probs = 0.975)
quantile(mod_bs$season1993, probs = 0.95)
quantile(mod_bs$season1993, probs = 0.05)
quantile(mod_bs$season1993, probs = 0.025)
mod_lm_sqrt2_org
sort(mod_bs$season1993)

quantile(mod_bs$season2003, probs = 0.975)
quantile(mod_bs$season2003, probs = 0.95)
quantile(mod_bs$season2003, probs = 0.05)
quantile(mod_bs$season2003, probs = 0.025)
mod_lm_sqrt2_org
sort(mod_bs$season2003)

quantile(mod_bs$season2013, probs = 0.975)
quantile(mod_bs$season2013, probs = 0.95)
quantile(mod_bs$season2013, probs = 0.05)
quantile(mod_bs$season2013, probs = 0.025)
mod_lm_sqrt2_org
sort(mod_bs$season2013)

###### bootstrap Insa
season <- as.factor(prep$season)

bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(coef(fit)) 
}

mod_bs_in <- boot(data = prep, statistic = bs, 
                  R = 2000, formula = prep$P_SQRT2 ~ sin_month_percent + 
                    cos_month_percent + season)

mod_bs_in <- as.data.frame(do.call("rbind", mod_bs_in)) # sample?

mod_bs_in
plot(mod_bs_in, index=1) # intercept 
plot(mod_bs_in, index=2) # sin_month_percent
plot(mod_bs_in, index=3) # cos_month_precent
plot(mod_bs_in, index=4) # season


boot.ci(mod_bs_in, index=1) 
boot.ci(mod_bs_in, type="bca", index=2) 
boot.ci(mod_bs_in, type="bca", index=3) 
boot.ci(mod_bs_in, index=4)

mod_lm_sqrt2_org
########################################


################################
##
## Version Alberich
##
################################
test<-cosinor(P_MEAN_NEW ~ 1 + as.factor(season), date = "YEAR", data = prep)
test
summary(test)
# Achtung:
# Stationaritdt 
# Symmetrisches 

#andere Variante
t <-  as.numeric(prep$mnth)/12
sint <- sin(2*pi*t)
cost <- cos(2*pi*t)
test_f<-lm(prep$P_MEAN_NEW ~ sint + cost + as.factor(prep$season))
summary(test_f) 
#Vergleiche estimate von test und test_f f|r Faktor as.factor(seasonal_precip$SEASON)
#Vergleiche intercept



################################
##
## Version Mausi
##
################################

# Signifikanz der Dekanden als erklaerende Variable ----------------------------
# Lineares Model mit wurzeltransformiertem Niederschlag
p <- prep$P_MEAN_NEW**0.25
qqnorm(diff(p))

t <-  prep$mnth/12
f <- prep$season
sint <- sin(2*pi*t)
cost <- cos(2*pi*t)
sinf <- sin(2*pi*f)
cosf <- cos(2*pi*f)
l <- lm(p ~ sint + cost + sinf + cosf + f)
summary(l)


# GLM mit nicht-transformiertem Niederschlag
g <- glm(prep$P_MEAN_NEW ~ t + f)
summary(g)
gf <- glm(prep$P_MEAN_NEW ~ sint + cost + as.factor(f))
summary(gf)

# Library season ---------------------------------------------------------------
# Nur 1973-1982
p73 <- prep$P_MEAN_NEW[prep$season == "1973"]
m73 <- prep$mnth[prep$season == "1973"]
s73 <- prep$season[prep$season == "1973"]
df73 <- data.frame(P = p73, mnth = m73, season = s73)


# Nur 2003-2012
p03 <- prep$P_MEAN_NEW[prep$season == "2003"]
m03 <- prep$mnth[prep$season == "2003"]
s03 <- prep$season[prep$season == "2003"]
df03 <- data.frame(P = p03, mnth = m03, season = s03)

# Vergleich Amplitude und Phase (= Peak) fuer beide Dekaden
cnr73 <- cosinor(P ~ season, date = "mnth", data = df73, type = 'monthly', family = gaussian())
cnr03 <- cosinor(P ~ season, date = "mnth", data = df03, type = 'monthly', family = gaussian())

summary(cnr73)
summary(cnr03)

# Quantile regression ----------------------------------------------------------
# Quantile
m <- prep$YEAR + prep$mnth_precent

reg15 <- rq(prep$P_MEAN_NEW ~ m, tau = c(0.15))
summary(reg15, se = "nid")

reg95 <- rq(prep$P_MEAN_NEW ~ m, tau = c(0.95))
summary(reg95, se = "nid")


plot(prep$P_MEAN_NEW ~ m)
abline(reg95, col = "blue")
abline(reg15, col = "green")

