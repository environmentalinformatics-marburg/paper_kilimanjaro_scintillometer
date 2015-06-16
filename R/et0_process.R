source("R/slsPkgs.R")
source("R/slsFcts.R")
source("R/visRainfall.R")
source("R/volumeFromLevelChange.R")

df_et0 <- read.csv("../../phd/scintillometer/evaporation_pan/data.csv")
df_et0$Plot <- tolower(df_et0$Plot)
df_et0$Plot <- factor(df_et0$Plot, levels = slsPlots(style = "ggplot"))
df_et0$Datetime <- as.POSIXct(df_et0$Datetime, format = "%Y-%m-%d %H:%S")
df_et0$B1 <- df_et0$B1 * .1
df_et0$B2 <- df_et0$B2 * .1
df_et0 <- subset(df_et0, Season == "r")

df_et0 %>%
  group_by(Plot) %>%
  mutate(B = (B1 + B2) / 2) %>%
  mutate(DIFF = c(0, diff(B))) %>%
  filter(DIFF != 0) %>%
  data.frame() -> df_et0

## remove first gra2 measurement
dt_seq <- seq(as.Date("2014-03-06"), as.Date("2014-03-08"), "day")
df_et0 <- df_et0[!as.Date(df_et0$Datetime) %in% dt_seq, ]

## rainfall data
df_rf <- visRainfall(return_data = TRUE, visualize = FALSE, ssn = "r")

## rainfall-corrected evapotranspiration per plot
ch_plot <- unique(df_et0$Plot)

ls_et0 <- lapply(ch_plot, function(h) {
  df_et0_sub <- subset(df_et0, Plot == h)
  df_rf_sub <- subset(df_rf, plot == h)
  
  if (nrow(df_et0_sub) == 0)
    return(invisible())
  
  ls_et0_sub <- lapply(1:nrow(df_et0_sub), function(i) {
    tmp_df <- subset(df_rf_sub, datetime < df_et0_sub$Datetime[i])
    tmp_num_rf <- sum(tmp_df$variable, na.rm = TRUE)
    
    tmp_num_diff <- df_et0_sub$DIFF[i]
    tmp_num_mm <- volumeFromLevelChange(tmp_num_diff, to_mm = TRUE)
    
    tmp_df_out <- df_et0_sub[i, ]
    tmp_df_out$DIFF_MM <- tmp_num_mm
    tmp_df_out$DIFF_WO_RAIN <- tmp_df_out$DIFF_MM - tmp_num_rf
    return(tmp_df_out)
  })
  df_et0_sub <- do.call("rbind", ls_et0_sub)
})
df_et0 <- do.call("rbind", ls_et0)

df_et0 %>% 
  group_by(Plot) %>%
  summarize(MEAN = mean(DIFF_WO_RAIN)) %>%
  data.frame()


## visualize
ggplot(aes(x = Datetime, y = DIFF_WO_RAIN), data = df_et0) + 
  geom_histogram(stat = "identity", drop = TRUE) + 
  facet_wrap(~ Plot, scales = "free_x", ncol = 2) + 
  labs(x = "Time (days)", y = expression(ET[0]))
