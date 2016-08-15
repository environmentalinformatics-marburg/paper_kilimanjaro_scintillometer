### environmental stuff --------------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## required functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

ch_dir_pub <- "../../pub/papers/detsch_et_al__spotty_evapotranspiration/"
ch_dir_tbl <- "../../phd/scintillometer/data/tbl/"

## input data
df_fls <- slsAvlFls()

## radiation
df_rad <- summarizeVar(df_fls$mrg_rf_agg01h, param = "netRad", ssn = df_fls$season,
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_rad.csv"))

## p
df_p <- summarizeVar(df_fls$mrg_rf_agg01h, param = "pressure", ssn = df_fls$season,
                     FUN_dy = function(...) mean(..., na.rm = TRUE),
                     file_out = paste0(ch_dir_tbl, "table_p.csv"))

## S
df_s <- summarizeVar(df_fls$mrg_rf_agg01h, param = "soilHeatFlux", ssn = df_fls$season,
                     FUN_dy = function(...) mean(..., na.rm = TRUE),
                     file_out = paste0(ch_dir_tbl, "table_S.csv"))

## vpd
df_vpd <- summarizeVar(df_fls$mrg_rf_agg01h, param = "vpd", ssn = df_fls$season,
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_vpd.csv"))

## et
df_et <- summarizeVar(df_fls$mrg_rf_agg01h, ssn = df_fls$season,
                      file_out = paste0(ch_dir_tbl, "table_et.csv"))

## plot coordinates
spt_plot <- readOGR("/media/fdetsch/Permanent/kilimanjaro/coordinates/", 
                    "PlotPoles_ARC1960_mod_20140807_final", 
                    p4s = "+init=epsg:21037")
spt_plot <- subset(spt_plot, PoleType == "AMP")

## merge data
df_var <- Reduce(function(...) merge(..., by = c("PlotID", "season")), 
                 list(df_rad, df_p, df_s, df_vpd, df_et))
df_var_ele <- merge(df_var, spt_plot@data, by = "PlotID")
df_var_ele$habitat <- substr(df_var_ele$PlotID, 1, 3)

id <- unlist(sapply(slsPlots(style = "elevation"), function(i) {
  tmp <- grep(i, df_var_ele$PlotID)
  if (length(tmp) == 2) {
    return(tmp[2:1])
  } else {
    return(tmp)
  }
}))
df_var_ele <- df_var_ele[id, ]

## point shapes
pch <- c(24, 25, 24, 25, 23, 23, 22, 22, 21, 21, 3, 13, 4)

## colors
clr <- c(rep("grey50", 2), rep("white", 2), rep(c("grey50", "white"), 3), 
         rep("black", 3))
# cols <- envinmr.theme()$topo.cols(8)[1:7]
# names(cols) <- c("sav", "mai", "cof", "gra", "fed", "hel", "fer")
# 
# ## focal plots
# df_var_ele$focal <- "yes"
# df_var_ele$focal[df_var_ele$PlotID %in% c("gra2", "cof2", "mai4", "sav5")] <- "no"

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

p_rad <- ggplot(data = df_var_ele) + 
  geom_point(aes(x = netRadfun, y = Z_DEM_HMP), fill = clr, shape = pch, 
             size = 4) +   
  labs(title = expression("a) R"[net] ~ "(W/" * m^{2} * ")"), x = "", y = "") + 
  theme_bw() + 
  theme(title = element_text(size = 8), 
        text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        legend.position = "none")

p_p <- ggplot(data = df_var_ele) + 
  geom_point(aes(x = pressurefun, y = Z_DEM_HMP), fill = clr, shape = pch, 
             size = 4) +   
  scale_y_continuous(labels = NULL) + 
  labs(title = "b) p (hPa)", x = "", y = "") + 
  theme_bw() + 
  theme(title = element_text(size = 8), 
        text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        legend.position = "none")

p_s <- ggplot(data = df_var_ele) + 
  geom_point(aes(x = soilHeatFluxfun, y = Z_DEM_HMP), fill = clr, shape = pch, 
             size = 4) +   
  scale_y_continuous(labels = NULL) + 
  labs(title = expression("c) S" ~ "(W/" * m^{2} * ")"), x = "", y = "") + 
  theme_bw() + 
  theme(title = element_text(size = 8), 
        text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        legend.position = "none")

p_vpd <- ggplot(data = df_var_ele) + 
  geom_point(aes(x = vpdfun, y = Z_DEM_HMP), fill = clr, shape = pch, 
             size = 4) +   
  labs(title = expression("d) VPD (Pa)"), x = "", y = "") + 
  theme_bw() + 
  theme(title = element_text(size = 8), 
        text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        legend.position = "none")

p_et <- ggplot(data = df_var_ele) + 
  geom_point(aes(x = waterETfun, y = Z_DEM_HMP), fill = clr, shape = pch, 
             size = 4) +   
  scale_y_continuous(labels = NULL) + 
  labs(title = expression("e) ET (mm)"), x = "", y = "") + 
  theme_bw() + 
  theme(title = element_text(size = 8), 
        text = element_text(size = 10), 
        axis.title.x = element_text(angle = 180), 
        axis.title.y = element_text(angle = 90), 
        legend.position = "none")

## legend
df_var_ele$plot_season <- as.character(df_var_ele$PlotID)
df_var_ele[df_var_ele$season == "d", "plot_season"] <- 
  paste(df_var_ele[df_var_ele$season == "d", "plot_season"], "(d)")
levels <- slsPlots(style = "elevation")
levels <- c(levels[1], "sav5 (d)", levels[2], "sav0 (d)", levels[3:length(levels)])
df_var_ele$plot_season <- factor(df_var_ele$plot_season, levels = levels)

p_key_ele <- ggplot(data = df_var_ele) + 
  geom_point(aes(y = vpdfun, x = Z_DEM_HMP, fill = plot_season, 
             shape = plot_season), size = 4) +   
  scale_fill_manual(expression(bold("PlotID")), guide = guide_legend(ncol = 2, byrow = TRUE), 
                    values = c("sav5" = clr[1], "sav5 (d)" = clr[2],
                               "sav0" = clr[3], "sav0 (d)" = clr[4],
                               "mai0" = clr[6], "mai4" = clr[5], 
                               "cof3" = clr[7], "cof2" = clr[8], 
                               "gra1" = clr[9], "gra2" = clr[10], 
                               "fer0" = clr[13], "fed1" = clr[11], 
                               "hel1" = clr[12]), 
                    breaks = c("sav5", "sav5 (d)", "sav0", "sav0 (d)", 
                               "mai4", "mai0", "cof3", "cof2", "gra1", "gra2", 
                               "fed1", "hel1", "fer0")) + 
  scale_shape_manual(expression(bold("PlotID")), 
                     values = c("sav5" = pch[1], "sav5 (d)" = pch[2], 
                                "sav0" = pch[3], "sav0 (d)" = pch[4],
                                "mai4" = pch[5], "mai0" = pch[6], 
                                "cof3" = pch[7], "cof2" = pch[8], 
                                "gra1" = pch[9], "gra2" = pch[10], 
                                "fed1" = pch[11], "hel1" = pch[12], 
                                "fer0" = pch[13]), 
                     breaks = c("sav5", "sav5 (d)", "sav0", "sav0 (d)", 
                                "mai4", "mai0", "cof3", "cof2", "gra1", "gra2", 
                                "fed1", "hel1", "fer0")) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  theme_bw() + 
  theme(text = element_text(size = 10), 
        legend.key.size = unit(.6, "cm"))

## save arranged plots incl. customized legend
legend <- ggExtractLegend(p_key_ele) 
ls_p <- list(p_rad, p_p, p_s, p_vpd, p_et)


## standalone tiff version
setEPS()
postscript(paste0(ch_dir_pub, "fig/figure04.eps"), width = 19*.3937, 
           height = 16*.3937)

grid.newpage()
n <- 0
for (y in c(0.5, 0)) {
  for (x in seq(.02, .66, .32)) {
    n <- n + 1
    
    if (n > length(ls_p)) 
      break
    
    # insert plots
    height <- if (n == 2) {
      .49
    } else {
      .5
    }
    
    vp_tmp <- viewport(x = ifelse(x == .34, x + .02, x), y = y, 
                       width = ifelse(x == .02, .33, .3),
                       height = height, just = c("left", "bottom"))
    pushViewport(vp_tmp)
    print(ls_p[[n]], newpage = FALSE)
    
    upViewport()
  }
}

vp_legend <- viewport(x = .725, y = .15, width = .2, height = .25, 
                      just = c("left", "bottom"))
pushViewport(vp_legend)
grid.draw(legend)

# y axis title
upViewport(0)
vp_yaxis <- viewport(x = .02, y = .55, width = .1, height = 1, angle = 90)
pushViewport(vp_yaxis)
grid.text("Elevation (m a.s.l.)", gp = gpar(cex = .8))

dev.off()
