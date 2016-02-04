### environmental stuff --------------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

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
spt_plot <- readOGR("/media/permanent/kilimanjaro/coordinates/", 
                    "PlotPoles_ARC1960_mod_20140807_final", 
                    p4s = "+init=epsg:21037")
spt_plot <- subset(spt_plot, PoleType == "AMP")

## merge data
df_var <- Reduce(function(...) merge(..., by = "PlotID"), 
                 list(df_ta, df_rh, df_vpd, df_rad, df_et))
df_var_ele <- merge(df_var, spt_plot@data, by = "PlotID")
df_var_ele$habitat <- substr(df_var_ele$PlotID, 1, 3)

## colors
cols <- envinmr.theme()$topo.cols(8)[1:7]
names(cols) <- c("sav", "mai", "cof", "gra", "fed", "hel", "fer")

# cols_upper <- brewer.pal(3, "YlOrBr")
# names(cols_upper) <- c("hel", "fed", "fer")
# cols <- c("sav" = "yellow", "mai" = "darkgreen", 
#           "cof" = "chocolate4", "gra" = "green")
# cols <- c(cols, cols_upper)

## focal plots
df_var_ele$focal <- "yes"
df_var_ele$focal[df_var_ele$PlotID %in% c("gra2", "cof2", "mai4", "sav5")] <- "no"

# ## statistics (trend profiles)
# lm_ta <- lm(tempUpfun ~ Z_DEM_HMP, data = df_var_ele)
# summary(lm_ta)
# 
# lm_vpd <- lm(vpdfun ~ Z_DEM_HMP, data = df_var_ele)
# summary(lm_vpd)
# 
# loe_rad <- loess(dwnRadfun ~ Z_DEM_HMP, data = df_var_ele, span = .99)
# summary(loe_rad)
# 
# loe_et <- loess(waterETfun ~ Z_DEM_HMP, data = df_var_ele, span = .99)
# hat <- predict(loe_et)
# cor(df_var_ele$waterETfun, hat)^2
# summary(loe_et)

## ta
p_ta_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = tempUpfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression("a) T"["a-150"] ~ "(" * degree * C * ")"), x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## rh
p_rh_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = humidityfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500), 
                     labels = NULL) + 
  labs(y = expression("b) rH (%)"), x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## rad
p_rad_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = dwnRadfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression("d) R"[dwn] ~ "(W/" * m^{2} * ")"), x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## et
p_et_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = waterETfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500), 
                     labels = NULL) + 
  ylim(1.5, 5) + 
  labs(y = expression("e) ET (mm)"), x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## vpd
p_vpd_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = vpdfun, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500), 
                     labels = NULL) + 
  ylim(50, 720) + 
  labs(y = expression("c) VPD (Pa)"), x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## legend
cols_key <- unname(cols)
p_key_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = vpdfun, x = Z_DEM_HMP, fill = PlotID, shape = PlotID), 
             colour = "black", size = 4) +   
  scale_fill_manual("Sampling plots", guide = guide_legend(nrow = 1, byrow = TRUE), 
                    values = c("sav0" = cols_key[1], "sav5" = cols_key[1], 
                               "mai0" = cols_key[2], "mai4" = cols_key[2], 
                               "cof3" = cols_key[3], "cof2" = cols_key[3], 
                               "gra1" = cols_key[4], "gra2" = cols_key[4], 
                               "fer0" = cols_key[7], "fed1" = cols_key[5], 
                               "hel1" = cols_key[6]), 
                    breaks = c("sav5", "sav0", "mai4", "mai0",   
                               "cof3", "cof2", "gra1", "gra2", 
                               "fed1", "hel1", "fer0")) + 
  scale_shape_manual("Sampling plots", values = c("sav0" = 23, "sav5" = 22, 
                                "mai0" = 23, "mai4" = 22, 
                                "cof3" = 23, "cof2" = 22, 
                                "gra1" = 23, "gra2" = 22, 
                                "fer0" = 23, "fed1" = 23, 
                                "hel1" = 23), 
                     breaks = c("sav5", "sav0", "mai4", "mai0",   
                                "cof3", "cof2", "gra1", "gra2", 
                                "fed1", "hel1", "fer0")) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        legend.key.size = unit(.5, "cm"))

## save arranged plots incl. customized legend
source("R/fig05__lai_ele.R")
legend <- ggExtractLegend(p_key_ele) 
ls_p <- list(p_ta_ele, p_rh_ele, p_vpd_ele, p_rad_ele, p_et_ele, p_licor_ele)

png(paste0(ch_dir_pub, "fig/figure05.png"), width = 17, height = 20, 
    units = "cm", res = 500)

grid.newpage()
n <- 0
for (x in c(0, 0.45)) {
  for (y in seq(0.02, 0.66, 0.32)) {
    n <- n + 1
    
    # insert plots
    vp_tmp <- viewport(x = x, y = ifelse(y == .02, y, y + .02), width = .455, 
                       height = ifelse(y == .02, .34, .31), 
                       just = c("left", "bottom"))
    pushViewport(vp_tmp)
    print(ls_p[[n]], newpage = FALSE)
    
    upViewport()
  }
}

vp_legend <- viewport(x = .95, y = .5, width = .1, height = .5, angle = 90)
pushViewport(vp_legend)
grid.draw(legend)

# y axis title
upViewport(0)
vp_yaxis <- viewport(x = 0, y = -.02, width = 1, height = .1, 
                     just = c("left", "bottom"))
pushViewport(vp_yaxis)
grid.text("Elevation (m a.s.l.)", rot = 180, gp = gpar(cex = .8))

dev.off()

## standalone tiff version
setEPS()
postscript(paste0(ch_dir_pub, "fig/figure05.eps"), width = 17*.3937, 
           height = 20*.3937)

grid.newpage()
n <- 0
for (x in c(0, 0.45)) {
  for (y in seq(0.02, 0.66, 0.32)) {
    n <- n + 1
    
    # insert plots
    vp_tmp <- viewport(x = x, y = ifelse(y == .02, y, y + .02), width = .455, 
                       height = ifelse(y == .02, .34, .31), 
                       just = c("left", "bottom"))
    pushViewport(vp_tmp)
    print(ls_p[[n]], newpage = FALSE)
    
    upViewport()
  }
}

vp_legend <- viewport(x = .95, y = .5, width = .1, height = .5, angle = 90)
pushViewport(vp_legend)
grid.draw(legend)

# y axis title
upViewport(0)
vp_yaxis <- viewport(x = 0, y = -.02, width = 1, height = .1, 
                     just = c("left", "bottom"))
pushViewport(vp_yaxis)
grid.text("Elevation (m a.s.l.)", rot = 180, gp = gpar(cex = .8))

dev.off()
