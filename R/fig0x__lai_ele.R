## required functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

ch_dir_pub <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"
ch_dir_tbl <- "../../phd/scintillometer/data/tbl/"

## input data
df_fls <- slsAvlFls(ssn = "r")

## lai
load("data/modis_lai_mu.rds")
df_lai <- df_sls_lai_md
df_lai <- subset(df_lai, season == "r")

## gpp
load("data/modis_gpp_mu.rds")
df_gpp <- df_sls_gpp_md
df_gpp <- subset(df_gpp, season == "r")

## plot coordinates
spt_plot <- readOGR("/media/permanent/kilimanjaro/coordinates/coords/", 
                    "PlotPoles_ARC1960_mod_20140807_final", 
                    p4s = "+init=epsg:21037")
spt_plot <- subset(spt_plot, PoleType == "AMP")

## merge data
df_var <- Reduce(function(...) merge(..., by = c("plot", "season", "begin", "end")), 
                                     list(df_lai, df_gpp))
df_var_ele <- merge(spt_plot@data, df_var, by.x = "PlotID", by.y = "plot")
df_var_ele$habitat <- substr(df_var_ele$PlotID, 1, 3)

cols_upper <- brewer.pal(3, "YlOrBr")
names(cols_upper) <- c("hel", "fed", "fer")
cols <- c("sav" = "yellow", "mai" = "darkgreen", 
          "cof" = "chocolate4", "gra" = "green")
cols <- c(cols, cols_upper)

df_var_ele$focal <- "yes"
df_var_ele$focal[df_var_ele$PlotID %in% c("gra2", "cof2", "mai4", "sav5")] <- "no"

## lai
p_lai_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = lai, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression(atop("LAI", "")), 
       x = "Elevation (m)\n") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## gpp
p_gpp_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = gpp, x = Z_DEM_HMP, fill = habitat, shape = focal), 
             colour = "black", size = 6) +   
  scale_fill_manual(values = cols) + 
  scale_shape_manual(values = c("yes" = 23, "no" = 22)) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  labs(y = expression(atop("GPP (kgC/" ~ m^{2} * ")", "")), x = "") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

## legend
p_key_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = lai, x = Z_DEM_HMP, fill = PlotID, shape = PlotID), 
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

png(paste0(ch_dir_pub, "fig/fig0x_lai_mu_ele.png"), width = 11.25, height = 25, 
    units = "cm", pointsize = 15, res = 300)
grid.newpage()

vp_figure <- viewport(x = 0, y = 0, width = 1, height = 2/3, 
                      just = c("left", "bottom"))
pushViewport(vp_figure)
grid.arrange(p_gpp_ele, p_lai_ele, as.table = TRUE, newpage = FALSE)

upViewport()
vp_legend <- viewport(x = 0.5, y = 0.76, width = .3, height = .3, angle = 90)
pushViewport(vp_legend)
grid.draw(legend)

dev.off()
