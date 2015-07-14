## required functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

ch_dir_pub <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"
ch_dir_tbl <- "../../phd/scintillometer/data/tbl/"

## input data
df_fls <- slsAvlFls(ssn = "r")

## ta
df_ta <- summarizeVar(df_fls$mrg_rf_agg01h, param = "tempUp", 
                      FUN_dy = function(...) mean(..., na.rm = TRUE),
                      file_out = paste0(ch_dir_tbl, "table_ta.csv"))

## rh
df_rh <- summarizeVar(df_fls$mrg_rf_agg01h, param = "humidity", 
                      FUN_dy = function(...) mean(..., na.rm = TRUE),
                      file_out = paste0(ch_dir_tbl, "table_rh.csv"))

## radiation
df_rad <- summarizeVar(df_fls$mrg_rf_agg01h, param = "dwnRad", 
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_rad.csv"))

## et
df_et <- summarizeVar(df_fls$mrg_rf_agg01h, 
                      file_out = paste0(ch_dir_tbl, "table_et.csv"))

## vpd
df_vpd <- summarizeVar(df_fls$mrg_rf_agg01h, param = "vpd", 
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_vpd.csv"))

## plot coordinates
spt_plot <- readOGR("/media/permanent/kilimanjaro/coordinates/coords/", 
                    "PlotPoles_ARC1960_mod_20140807_final", 
                    p4s = "+init=epsg:21037")
spt_plot <- subset(spt_plot, PoleType == "AMP")

## merge data
df_var <- Reduce(function(...) merge(..., by = "PlotID"), 
                 list(df_ta, df_rh, df_vpd, df_rad, df_et))
df_var_ele <- merge(df_var, spt_plot@data, by = "PlotID")
df_var_ele$habitat <- substr(df_var_ele$PlotID, 1, 3)

cols_upper <- brewer.pal(3, "YlOrBr")
names(cols_upper) <- c("hel", "fed", "fer")
cols <- c("sav" = "yellow", "mai" = "darkgreen", 
          "cof" = "chocolate4", "gra" = "green")
cols <- c(cols, cols_upper)

df_var_ele$focal <- "yes"
df_var_ele$focal[df_var_ele$PlotID %in% c("gra2", "cof2", "mai4", "sav5")] <- "no"

## statistics (trend profiles)
lm_ta <- lm(tempUpfun ~ Z_DEM_HMP, data = df_var_ele)
summary(lm_ta)

lm_vpd <- lm(vpdfun ~ Z_DEM_HMP, data = df_var_ele)
summary(lm_vpd)

loe_rad <- loess(dwnRadfun ~ Z_DEM_HMP, data = df_var_ele, span = .99)
summary(loe_rad)

## ta
p_ta_ele <- ggplot(data = df_var_ele) + 
  stat_smooth(aes(y = tempUpfun, x = Z_DEM_HMP), se = FALSE, 
              method = "lm", span = .99, colour = "grey50", 
              linetype = "longdash", lwd = 2) + 
  geom_point(aes(y = tempUpfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression(atop("Temperature (" * degree * C * ")", "")), 
       x = "Elevation (m)\n") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## rh
p_rh_ele <- ggplot(data = df_var_ele) + 
  #   stat_smooth(aes(y = humidityfun, x = Z_DEM_HMP), se = FALSE, 
  #               method = "loess", span = .99, colour = "grey50", 
  #               linetype = "longdash", lwd = 2) + 
  geom_point(aes(y = humidityfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression(atop("Relative humidity (%)", "")), x = "") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## rad
p_rad_ele <- ggplot(data = df_var_ele) + 
  stat_smooth(aes(y = dwnRadfun, x = Z_DEM_HMP), se = FALSE, 
              method = "loess", span = .99, colour = "grey50", 
              linetype = "longdash", lwd = 2) + 
  geom_point(aes(y = dwnRadfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression(atop("Downward radiation (W/" * m^{2} * ")", " ")), 
       x = "Elevation (m)\n") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## et
p_et_ele <- ggplot(data = df_var_ele) + 
  stat_smooth(aes(y = waterETfun, x = Z_DEM_HMP), se = FALSE, 
              method = "loess", span = .99, colour = "grey50", 
              linetype = "longdash", lwd = 2) + 
  geom_point(aes(y = waterETfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  ylim(1.5, 5) + 
  labs(y = expression(atop("Evapotranspiration (mm/day)", "")), x = "") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## vpd
p_vpd_ele <- ggplot(data = df_var_ele) + 
  stat_smooth(aes(y = vpdfun, x = Z_DEM_HMP), se = FALSE, 
              method = "lm", span = .99, colour = "grey50", 
              linetype = "longdash", lwd = 2) + 
  geom_point(aes(y = vpdfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  ylim(50, 720) + 
  labs(y = expression(atop("Vapor pressure deficit (Pa)", "")), x = "") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## legend
p_key_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = vpdfun, x = Z_DEM_HMP, fill = PlotID, shape = PlotID), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = c("sav0" = "yellow", "sav5" = "yellow", 
                               "mai0" = "darkgreen", "mai4" = "darkgreen", 
                               "cof3" = "chocolate4", "cof2" = "chocolate4", 
                               "gra1" = "green", "gra2" = "green", 
                               "fer0" = "#D95F0E", "fed1" = "#FEC44F", 
                               "hel1" = "#FFF7BC"), 
                    breaks = c("fer0", "hel1", "fed1",  
                               "gra2", "gra1", "cof2", "cof3", 
                               "mai0", "mai4", "sav0", "sav5")) + 
  scale_shape_manual(values = c("sav0" = 23, "sav5" = 22, 
                                "mai0" = 23, "mai4" = 22, 
                                "cof3" = 23, "cof2" = 22, 
                                "gra1" = 23, "gra2" = 22, 
                                "fer0" = 23, "fed1" = 23, 
                                "hel1" = 23), 
                     breaks = c("fer0", "hel1", "fed1",  
                                "gra2", "gra1", "cof2", "cof3", 
                                "mai0", "mai4", "sav0", "sav5")) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  theme_bw() + 
  theme(text = element_text(size = 14))

## save arranged plots incl. customized legend
legend <- ggExtractLegend(p_key_ele) 

png(paste0(ch_dir_pub, "fig/fig0x_et_ele.png"), width = 22.5, height = 25, 
    units = "cm", pointsize = 15, res = 300)
grid.arrange(p_ta_ele, p_rad_ele, p_rh_ele, p_et_ele, p_vpd_ele,  
             as.table = TRUE)

vp_legend <- viewport(x = 0.76, y = 0.76, width = .3, height = .3, angle = 90)
pushViewport(vp_legend)
grid.draw(legend)

dev.off()
