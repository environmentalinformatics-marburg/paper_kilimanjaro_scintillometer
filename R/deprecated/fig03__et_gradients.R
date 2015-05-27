## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

# output path
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# sls plots and referring files
ch_sls_plt <- slsPlots()

df_sls_fls <- slsAvlFls()
df_sls_fls_rs <- subset(df_sls_fls, season == "r")

# 60-min et rates (mm/h)
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls_rs), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls_rs$mrg_rf[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls_rs$plot[i], habitat = df_sls_fls_rs$habitat[i],
             season = df_sls_fls_rs$season[i], tmp_df)
})
df_sls_dv_01h <- do.call("rbind", ls_sls_dv_01h)

# maximum hourly et rates per plot
library(dplyr)
df_sls_dv_01h %>%
  group_by(plot) %>% 
  filter(waterET == max(waterET)) %>%
  data.frame() %>%
  arrange(waterET)


# diurnal et amounts (mm)
ls_sls_dv_01d <- lapply(ls_sls_dv_01h, function(i) {
  tmp.df <- slsAggregate(fn = i, agg_by = 24, include_time = FALSE,
                         FUN = function(...) round(sum(..., na.rm = TRUE), 1))
  data.frame(plot = unique(i$plot), habitat = unique(i$habitat), 
             season = unique(i$season), tmp.df)
})
df_sls_dv_01d <- do.call("rbind", ls_sls_dv_01d)

# daytime subset
ch_sls_dv_01h_hr <- substr(as.character(df_sls_dv_01h$datetime), 1, 2)
int_sls_dv_01h_hr <- as.integer(ch_sls_dv_01h_hr)
int_sls_dv_01h_dt <- int_sls_dv_01h_hr >= 4 & int_sls_dv_01h_hr < 20
df_sls_dv_01h_dt <- df_sls_dv_01h[int_sls_dv_01h_dt, ]

df_sls_dv_01h_dt$time <- strptime(df_sls_dv_01h_dt$datetime, format = "%H:%M:%S")
df_sls_dv_01h_dt$time_fac <- factor(format(df_sls_dv_01h_dt$time, format = "%H:%M"))

# reorder habitat factor levels
df_sls_dv_01h_dt$habitat_new <- factor(df_sls_dv_01h_dt$habitat, 
                                       levels = rev(c("mai", "sav", "cof", "gra", 
                                                      "fed", "fer", " ", "hel")))

# x-axis labels
ch_lvl <- levels(df_sls_dv_01h_dt$time_fac)
ch_lbl <- rep("", length(ch_lvl))

ls_lvl <- strsplit(ch_lvl, ":")
ch_lvl_hr <- sapply(ls_lvl, "[[", 1)
int_lvl_hr <- as.integer(ch_lvl_hr)
int_lvl_hr_odd <- int_lvl_hr %% 2 != 0
ch_lvl_min <- sapply(ls_lvl, "[[", 2)

int_lvl_hr_odd_full <- ch_lvl_min == "00" & int_lvl_hr_odd
ch_lbl[int_lvl_hr_odd_full] <- ch_lvl[int_lvl_hr_odd_full]
names(ch_lbl) <- ch_lvl

# visualization
ch_cols_bg <- c(rep(c("black", "grey60"), 4), rep("black", 3))
names(ch_cols_bg) <- ch_sls_plt

p <- ggplot(aes(x = time_fac, y = waterET, group = plot, colour = plot, fill = plot), 
       data = df_sls_dv_01h_dt) + 
  geom_histogram(stat = "identity", position = "dodge") +
  facet_wrap(~ habitat_new, ncol = 2, drop = FALSE) + 
  scale_x_discrete(labels = ch_lbl) + 
  scale_color_manual("", values = ch_cols_bg) + 
  scale_fill_manual("", values = ch_cols_bg) + 
  guides(colour = FALSE, fill = FALSE) + 
  labs(x = "\nTime (hours)", y = "Evapotranspiration (mm) \n") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

png(paste0(ch_dir_ppr, "/fig/fig03__elev_dist_gradients.png"), width = 35, 
    height = 40, units = "cm", res = 300, pointsize = 18)
print(p) 
dev.off()
