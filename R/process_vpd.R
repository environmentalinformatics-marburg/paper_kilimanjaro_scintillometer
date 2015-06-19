## required packages and functions
source("R/slsPkgs.R") 
source("R/slsFcts.R")
source("R/vpd.R")

df_fls <- slsAvlFls(ch_pattern = "mrg_rf_agg10m.csv$")

## maize subset
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

## elevation profile
df_fls <- slsAvlFls()

source("R/summarizeVar.R")
df_vpd <- summarizeVar(df_fls$mrg_rf_agg01h)

ls_vpd <- lapply(df_fls[, "mrg_rf_agg01h"], function(i) {
  tmp_ch_plot <- substr(basename(i), 1, 4)
  
  ## import, reformat and merge daily data
  tmp_df <- read.csv(i, stringsAsFactors = FALSE)
  tmp_df <- tmp_df[, c("datetime", "vpd")]
  tmp_df[, "Time"] <- sapply(strsplit(tmp_df[, "datetime"], " "), "[[", 2)
  tmp_ls <- split(tmp_df, yday(tmp_df$datetime))
  tmp_df_mrg <- Reduce(function(...) merge(..., by = "Time"), tmp_ls)
  tmp_df_mrg <- tmp_df_mrg[, -grep("datetime", names(tmp_df_mrg))]
  
  ## hourly means (mu) and standard errors (se)
  tmp_num_mu <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) mean(..., na.rm = TRUE))
  tmp_num_se <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) std.error(..., na.rm = TRUE))
  
  ## merge and return relevant data
  tmp_df_all <- data.frame(PlotID = tmp_ch_plot, Time = tmp_df_mrg[, "Time"], 
                           VPDmu = tmp_num_mu, VPDse = tmp_num_se)
  return(tmp_df_all)
})
df_vpd <- do.call("rbind", ls_vpd)

## maximum hourly and daily et rates per plot (table 3)
df_vpd %>%
  dplyr::select(PlotID, Time, VPDmu) %>%
  group_by(PlotID) %>% 
  filter(VPDmu == max(VPDmu)) %>%
  mutate(VPDmax = round(VPDmu, 2)) %>%
  dplyr::select(PlotID, VPDmax, Time) %>%
  data.frame() %>%
  arrange(desc(VPDmax)) -> df_max_hr

df_vpd %>%
  dplyr::select(PlotID, Time, VPDmu) %>%
  group_by(PlotID) %>% 
  summarise(sumVPDmu = round(mean(VPDmu, na.rm = TRUE), 2)) %>%
  data.frame() %>%
  arrange(desc(sumVPDmu)) -> df_max_dy

df_max <- merge(df_max_dy, df_max_hr, by = "PlotID", sort = FALSE)
write.csv(df_max, "../../phd/scintillometer/data/tbl/table_3|1.csv")

})