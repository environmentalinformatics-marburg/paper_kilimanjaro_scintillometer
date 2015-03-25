## environmental stuff

# packages
library(ggplot2)
library(raster)
library(rgdal)
library(plyr)

# functions
source("R/slsPlots.R")

# path: climatological data
ch_dir_data <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/"

# path: coordinates
ch_dir_crd <- "../../kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data

# throughfall: fer0
df_rf_fer0 <- read.csv(paste0(ch_dir_data, "fer0_rainfall.csv"), 
                       stringsAsFactors = FALSE, na.strings = "Na")

ch_rf_fer0_month <- df_rf_fer0$datetime
ch_rf_fer0_month <- substr(ch_rf_fer0_month, 6, 7)
ls_rf_fer0_month <- list(ch_rf_fer0_month)

df_rf_fer0_av <- aggregate(df_rf_fer0$Rainfall, by = ls_rf_fer0_month, 
                           FUN = function(...) mean(..., na.rm = TRUE))
names(df_rf_fer0_av) <- names(df_rf_fer0)

ggplot(aes(x = datetime, y = Rainfall), data = df_rf_fer0_av) + 
  geom_histogram(stat = "identity", colour = "darkblue", fill = "darkblue") + 
  labs(x = "Time (months)", y = "Rainfall (mm)") + 
  theme_bw()


## ta_200 interpolated, cf. Mwangomo et al. (forthcoming)

# raster data
fls_ta <- list.files(paste0(ch_dir_data, "ta_200"), pattern = ".tif$", 
                     full.names = TRUE)
rst_ta <- stack(fls_ta)

# dates
ch_ta_month <- basename(fls_ta)
ls_ta_month <- strsplit(ch_ta_month, "_interp_")
ch_ta_month <- sapply(ls_ta_month, "[[", 2)
ch_ta_month <- substr(ch_ta_month, 6, 7)
ls_ta_month <- list(ch_ta_month)

# plots
spt_plt <- readOGR(ch_dir_crd, ch_fls_crd)
spt_plt <- subset(spt_plt, PoleType %in% "AMP")
spt_plt_sls <- subset(spt_plt, PlotID %in% slsPlots())

# value extraction
df_ta_plt <- extract(rst_ta, spt_plt_sls, df = TRUE)
df_ta_plt[, 1] <- spt_plt_sls$PlotID

# aggregation
ls_ta_agg <- lapply(1:nrow(df_ta_plt), function(i) {
  tmp_df_ta <- df_ta_plt[i, 2:ncol(df_ta_plt)]
  tmp_num_ta <- as.numeric(tmp_df_ta)
  tmp_df_ta_agg <- aggregate(tmp_num_ta, by = ls_ta_month, FUN = mean)
  names(tmp_df_ta_agg) <- c("Month", "Ta_200")
  data.frame(PlotID = df_ta_plt[i, 1], tmp_df_ta_agg)
})
df_ta_agg <- do.call("rbind", ls_ta_agg)
df_ta_agg$Month <- factor(df_ta_agg$Month)

# visualization
ggplot(aes(x = Month, y = Ta_200), data = df_ta_agg) + 
  geom_point() + 
  facet_wrap(~ PlotID)
