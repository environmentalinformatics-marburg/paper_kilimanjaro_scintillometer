ch_dir_pub <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"

ch_fls_et <- list.files("/mnt/windows/Permanent/phd/scintillometer/data/tbl", 
                        pattern = "^table_3", full.names = TRUE)

ls_et <- lapply(ch_fls_et, read.csv)
df_et <- do.call("rbind", ls_et)

spt_plot <- readOGR("/media/permanent/kilimanjaro/coordinates/coords/", 
                    "PlotPoles_ARC1960_mod_20140807_final")
spt_plot <- subset(spt_plot, PoleType == "AMP")

df_et_ele <- merge(df_et, spt_plot@data, by = "PlotID")
df_et_ele$habitat <- substr(df_et_ele$PlotID, 1, 3)

cols_upper <- brewer.pal(3, "YlOrBr")
names(cols_upper) <- c("hel", "fed", "fer")
cols <- c("sav" = "yellow", "mai" = "darkgreen", 
          "cof" = "chocolate4", "gra" = "green")
cols <- c(cols, cols_upper)

df_et_ele$hjust <- .5
df_et_ele$vjust <- -1.35

int_id_fer0 <- grep("fer0", df_et_ele$PlotID)
df_et_ele$hjust[int_id_fer0] <- 1.5
df_et_ele$vjust[int_id_fer0] <- .5

p_et_ele <- ggplot(data = df_et_ele) + 
  geom_point(aes(y = sumETmu, x = Z_DEM_HMP, colour = habitat), 
             shape = 15, size = 6) + 
  geom_point(aes(y = sumETmu, x = Z_DEM_HMP, colour = habitat), 
             shape = 0, size = 6, colour = "black") + 
  geom_text(aes(y = sumETmu, x = Z_DEM_HMP, label = PlotID, hjust = hjust, vjust = vjust), 
            colour = "black", angle = 90, fontface = "bold") + 
  stat_smooth(aes(y = sumETmu, x = Z_DEM_HMP), se = FALSE, 
              method = "loess", span = .99, colour = "grey50", 
              linetype = "dashed", lwd = 2) + 
  scale_colour_manual(values = cols) + 
  scale_x_continuous(trans = "reverse", breaks = seq(1000, 4000, 500)) + 
  ylim(1.5, 4.1) + 
  labs(y = "Evapotranspiration (mm/day)\n", x = "Elevation (m)\n") + 
  theme_bw() + 
  theme(axis.title.x = element_text(angle = 180, size = 14), 
        axis.title.y = element_text(angle = 90, size = 14), 
        axis.text.y = element_text(angle = 90, hjust = 0.5), 
        axis.text.x = element_text(angle = 90, vjust = .5), 
        legend.position = "none")

png(paste0(ch_dir_pub, "fig/fig0x_et_ele.png"), width = 20, height = 15, 
    units = "cm", pointsize = 15, res = 300)
print(p_et_ele)
dev.off()
