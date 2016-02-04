## required functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

ch_dir_pub <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"
ch_dir_tbl <- "../../phd/scintillometer/data/tbl/"

## input data
df_fls <- slsAvlFls(ssn = "r")

## lai (licor)
load("data/licor_lai.rds")
df_licor <- df_plt_lai
df_licor <- subset(df_licor, season == "r")
names(df_licor)[names(df_licor) == "LAI"] <- "lai_licor"

## lai
load("data/modis_lai.rds")
df_modis <- df_sls_lai_md
df_modis <- subset(df_modis, season == "r")
names(df_modis)[names(df_modis) == "lai"] <- "lai_modis"

## et
df_et <- summarizeVar(df_fls$mrg_rf_agg01h, 
                      file_out = paste0(ch_dir_tbl, "table_et.csv"))


## plot coordinates
spt_plot <- readOGR("/media/permanent/kilimanjaro/coordinates/", 
                    "PlotPoles_ARC1960_mod_20140807_final", 
                    p4s = "+init=epsg:21037")
spt_plot <- subset(spt_plot, PoleType == "AMP")
spt_plot@data <- spt_plot@data[, c("PlotID", "Z_DEM_HMP")]

## merge data
df_lai <- merge(df_licor, df_modis, by = c("plot", "season"))
df_lai_ele <- merge(spt_plot@data, df_lai, by.x = "PlotID", by.y = "plot")
df_lai_ele$habitat <- substr(df_lai_ele$PlotID, 1, 3)

## colors
cols <- envinmr.theme()$topo.cols(8)[1:7]
names(cols) <- c("sav", "mai", "cof", "gra", "fed", "hel", "fer")

## focal plots
df_lai_ele$focal <- "yes"
df_lai_ele$focal[df_lai_ele$PlotID %in% c("gra2", "cof2", "mai4", "sav5")] <- "no"

# ## statistics
# mod_lai <- lm(lai_modis ~ lai_licor, data = df_lai_ele)
# summary(mod_lai)
# 
# df_lai_et <- merge(df_lai, df_et, by.x = "plot", by.y = "PlotID")
# mod_licor_et <- lm(lai_licor ~ waterETfun, data = df_lai_et)
# summary(mod_licor_et)
# 
# mod_modis_et <- lm(lai_modis ~ waterETfun, data = df_lai_et)
# summary(mod_modis_et)
# 
# loe_lai <- loess(lai ~ Z_DEM_HMP, data = df_lai_ele, span = .99)
# hat <- predict(loe_lai)
# cor(df_lai_ele$lai, hat)^2
# 
# loe_gpp <- loess(gpp ~ Z_DEM_HMP, data = df_lai_ele, span = .99)
# hat <- predict(loe_gpp)
# cor(df_lai_ele$gpp, hat)^2
# 
# mod <- lm(lai ~ gpp, data = df_lai_ele)
# summary(mod)
# 
# df_lai_ele_et <- merge(df_lai_ele, df_et, by = "PlotID")
# mod_etlai <- lm(waterETfun ~ log(lai), data = df_lai_ele_et)
# summary(mod_etlai)
# 
# mod_etgpp <- lm(waterETfun ~ log(gpp), data = df_lai_ele_et)
# summary(mod_etgpp)

## LiCOR-LAI-elevation relationship
p_licor_ele <- ggplot(data = df_lai_ele) + 
  geom_point(aes(y = lai_licor, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 4) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500), 
                     labels = NULL) + 
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(1, 5, 2)) + 
  labs(y = "f) LAI", x = "") + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

# ## MODIS-LAI-elevation relationship
# p_modis_ele <- ggplot(data = df_lai_ele) + 
#   #   stat_smooth(aes(y = lai_modis, x = Z_DEM_HMP), se = FALSE, 
#   #               method = "loess", span = .99, colour = "grey60", 
#   #               linetype = "longdash", lwd = 2) + 
#   geom_point(aes(y = lai_modis, x = Z_DEM_HMP, fill = habitat, shape = focal), 
#              colour = "black", size = 6) +   
#   scale_fill_manual(values = cols) + 
#   scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
#   scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
#   scale_y_continuous(limits = c(0, 6.5), breaks = seq(0, 6, 2)) + 
#   labs(y = expression(atop("LAI"[MODIS], "")), x = "") + 
#   theme_bw() + 
#   theme(axis.title.x = element_text(angle = 180, size = 14), 
#         axis.title.y = element_text(angle = 90, size = 14), 
#         axis.text.y = element_text(angle = 90, hjust = 0.5), 
#         axis.text.x = element_text(angle = 90, vjust = .5), 
#         legend.position = "none")

# ## LiCOR-LAI vs. MODIS-LAI
# p_scatter <- ggplot(aes(x = lai_licor, y = lai_modis, fill = habitat, shape = focal), 
#        data = df_lai_ele) + 
#   geom_point(colour = "black", size = 6) + 
#   geom_text(label = "r² = 0.11", x = 5, y = 5) + 
#   scale_fill_manual(values = cols) + 
#   scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
#   scale_x_continuous(limits = c(0, 6.35), breaks = seq(0, 6, 1)) +
#   scale_y_continuous(limits = c(0, 6.35), breaks = seq(0, 6, 1)) +
#   labs(x = expression(atop("", "LAI"[LI-COR])), 
#        y = expression(atop("LAI"[MODIS], ""))) + 
#   theme_bw() + 
#   theme(legend.position = "none")

# ## save arranged plots incl. customized legend
# png(paste0(ch_dir_pub, "fig/fig05__lai_ele.png"), width = 11.25*1.1, 
#     # height = 12*1.5, 
#     height = 6 * 1.5, units = "cm", pointsize = 12, res = 300)
# grid.newpage()
# 
# vp_figure <- viewport(x = 0, y = .05, width = 1, height = .95, 
#                       just = c("left", "bottom"))
# pushViewport(vp_figure)
# # grid.arrange(p_modis_ele, p_licor_ele, as.table = TRUE, newpage = FALSE)
# print(p_licor_ele, newpage = FALSE)
# 
# # upViewport()
# # vp_scatter <- viewport(x = 1, y = 0, width = .75, height = .6, 
# #                        just = c("left", "bottom"), angle = 90)
# # pushViewport(vp_scatter)
# # print(p_scatter, newpage = FALSE)
#  
# upViewport()
# vp_yaxis <- viewport(x = 0, y = 0, width = 1, height = .1, 
#                      just = c("left", "bottom"))
# pushViewport(vp_yaxis)
# grid.text("Elevation (m a.s.l.)", rot = 180)
# 
# dev.off()
