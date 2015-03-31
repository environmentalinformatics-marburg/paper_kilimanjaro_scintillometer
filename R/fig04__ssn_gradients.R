# packages
library(lubridate)
library(ggplot2)

# functions
source("R/slsPlots.R")
source("R/slsAvlFls.R") 
source("R/slsDiurnalVariation.R")

# output path
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# sls plots and referring files
df_sls_fls <- slsAvlFls()
df_sls_fls_ds <- subset(df_sls_fls, habitat == "sav")

# 60-min et rates (mm/h)
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls_ds), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls_ds$mrg_rf[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls_ds$plot[i], habitat = df_sls_fls_ds$habitat[i],
             season = df_sls_fls_ds$season[i], tmp_df)
})
df_sls_dv_01h <- do.call("rbind", ls_sls_dv_01h)

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
int_sls_dv_01h_dt <- int_sls_dv_01h_hr >= 0 & int_sls_dv_01h_hr < 24
df_sls_dv_01h_dt <- df_sls_dv_01h[int_sls_dv_01h_dt, ]

df_sls_dv_01h_dt$time <- strptime(df_sls_dv_01h_dt$datetime, format = "%H:%M:%S")
df_sls_dv_01h_dt$time_fac <- factor(format(df_sls_dv_01h_dt$time, format = "%H:%M"))

df_sls_dv_01h_dt$plot_season <- paste(df_sls_dv_01h_dt$plot, 
                                      df_sls_dv_01h_dt$season, sep = "_")

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
ch_cols_bg <- rep(c("black", "grey65"), 2)
names(ch_cols_bg) <- unique(df_sls_dv_01h_dt$plot_season)

p <- ggplot(aes(x = time_fac, y = waterET, group = plot_season, 
                colour = plot_season, fill = plot_season), 
            data = df_sls_dv_01h_dt) + 
  geom_histogram(stat = "identity", position = "dodge") +
  facet_wrap(~ plot, ncol = 1, drop = FALSE) + 
  scale_x_discrete(labels = ch_lbl) + 
  scale_color_manual("", values = ch_cols_bg) + 
  scale_fill_manual("", values = ch_cols_bg) + 
  guides(colour = FALSE, fill = FALSE) + 
  labs(x = "\nTime (hours)", y = "Evapotranspiration (mm) \n") + 
  theme_bw()

png(paste0(ch_dir_ppr, "/fig/fig04__seasonal_gradients.png"), width = 25, 
    height = 20, units = "cm", res = 300, pointsize = 15)
print(p) 
dev.off()
