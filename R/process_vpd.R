## required packages and functions
source("R/slsPkgs.R") 
source("R/slsFcts.R")
source("R/vpd.R")

df_fls <- slsAvlFls(ch_pattern = "mrg_rf_agg10m.csv$")
df_fls_mai <- subset(df_fls, habitat == "mai")

ls_dat_mai <- lapply(1:nrow(df_fls_mai), function(i) {
  tmp_df_dat <- read.csv(df_fls_mai[i, "mrg_rf_agg10m"])
  data.frame(PlotID = df_fls_mai[i, "plot"], tmp_df_dat)
})
df_dat_mai <- do.call("rbind", ls_dat_mai)
df_dat_mai$datetime <- as.POSIXct(df_dat_mai$datetime, format = "%Y-%m-%d %H:%M:%S")

df_dat_mai %>%
  mutate(vpd = vpd(tempUp, humidity)) %>%
  data.frame() -> df_dat_mai

ggplot(aes(x = datetime, y = vpd), data = df_dat_mai) + 
  geom_line() + 
  facet_wrap(~ PlotID, ncol = 1, scales = "free_x") + 
  labs(x = "\nTime (hours)", y = "Vapor pressure deficit (hPa)\n") + 
  theme_bw() + 
  theme(text = element_text(size = 15))
