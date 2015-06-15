source("R/slsPkgs.R")
source("R/slsFcts.R")
source("R/evaporativeDemand.R")
source("R/visRainfall.R")

df_et0 <- read.csv("../../phd/scintillometer/evaporation_pan/data.csv")
df_et0$Plot <- tolower(df_et0$Plot)
df_et0$Plot <- factor(df_et0$Plot, levels = slsPlots(style = "ggplot"))
df_et0$Datetime <- as.POSIXct(df_et0$Datetime, format = "%Y-%m-%d %H:%S")
df_et0 <- subset(df_et0, Season == "r")

df_et0 %>%
  group_by(Plot) %>%
  mutate(B = (B1 + B2) / 2) %>%
  mutate(DIFF = c(0, diff(B))) %>%
  filter(DIFF != 0) %>%
  data.frame() -> df_et0

## remove first gra2 measurement
dt_seq <- seq(as.Date("2014-03-06"), as.Date("2014-03-08"), "day")
df_et0 <- df_et0[!df_et0$Datetime %in% dt_seq, ]

## rainfall data
df_rf <- visRainfall(return_data = TRUE, visualize = FALSE, ssn = "r")

df_et0_sub <- subset(df_et0, Plot == "cof2")
df_rf_sub <- subset(df_rf, plot == "cof2")

lapply(df_et0_sub$Datetime, function(i) {
  tmp_df <- subset(df_rf_sub, datetime < i)
  sum(tmp_df$variable, na.rm = TRUE)
})
head(df_rf)

## subtract rainfall from et0


ggplot(aes(x = Datetime, y = DIFF), data = df_et0) + 
  geom_histogram(stat = "identity", drop = TRUE) + 
  facet_wrap(~ Plot, scales = "free_x", ncol = 2)
