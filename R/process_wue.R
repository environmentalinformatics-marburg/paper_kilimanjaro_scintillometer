## environmental stuff

# packages
library(plyr)
library(dplyr)
library(plotrix)
library(ggplot2)

# functions
source("R/slsAvlFls.R")
source("R/slsPlots.R")
source("R/slsDiurnalVariation.R")
source("R/slsAggregate.R")

# path: load data
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"

# path: output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"


## data 

# 60-min et rates (mm/h)
df_sls_fls <- slsAvlFls()
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls$mrg_rf[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls$plot[i], habitat = df_sls_fls$habitat[i],
             season = df_sls_fls$season[i], tmp_df)
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

# et and gpp data
# load(paste0(ch_dir_out_agg01d, "df_sls_dv_01d.RData"))
load(paste0(ch_dir_out_agg01d, "df_sls_gpp_md.RData"))

# merge and calculate water-use efficiency
df_sls_gpp <- merge(df_sls_dv_01d, df_sls_gpp_md, by = c("plot", "season"), all = TRUE)
df_sls_gpp$wue <- df_sls_gpp$gpp / df_sls_gpp$waterET


## visualization

# reorder factor levels
ch_lvl <- substr(slsPlots(), 1, 3)
ch_lvl <- unique(ch_lvl)
df_sls_gpp$habitat <- factor(df_sls_gpp$habitat, levels = ch_lvl)

df_sls_gpp$plot <- factor(df_sls_gpp$plot, levels = slsPlots())

ggplot(aes(x = habitat, y = wue, group = plot, fill = plot), data = df_sls_gpp) + 
  geom_histogram(stat = "identity", position = "dodge", 
                 ,subset = .(season == "r"))

# standard errors
df_sls_gpp %>% 
  filter(season == "r") %>% 
  group_by(habitat) %>% 
  summarise(wue_mu = mean(wue), wue_se = std.error(wue)) %>%
  data.frame() -> df_wue_se

limits <- aes(ymax = wue_mu + wue_se, ymin = wue_mu - wue_se)
num_ylim <- c(0, max(df_wue_se$wue_mu + df_wue_se$wue_se, na.rm = TRUE) + .002)

p_wue <- ggplot(aes(x = habitat, y = wue_mu), data = df_wue_se) + 
  geom_histogram(stat = "identity", fill = "grey50", colour = "black", 
                 lwd = 1.2, alpha = .5) + 
  geom_errorbar(limits, position = "dodge", linetype = "dashed", 
                width = .2) + 
  labs(x = "\nHabitat type", y = expression("Average daily WUE (kgC/m"^"2"~")")) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14)) + 
  coord_cartesian(ylim = num_ylim)

png(paste0(ch_dir_ppr, "fig/fig05__wue.png"), width = 20, height = 15, 
    units = "cm", pointsize = 21, res = 600)
print(p_wue)
dev.off()

# dry season
df_sls_gpp %>% 
  filter(habitat == "sav") %>% 
  #   group_by(season) %>% 
  #   summarise(wue_mu = mean(wue), wue_se = std.error(wue)) %>%
  data.frame() -> df_wue_se_ds

levels(df_wue_se_ds$season) <- c("wet", "dry")

num_ylim <- c(0, max(df_wue_se_ds$wue, na.rm = TRUE) + .002)

p_wue_ds <- ggplot(aes(x = plot, y = wue, group = season, fill = season), 
       data = df_wue_se_ds) + 
  geom_histogram(stat = "identity", position = "dodge", 
                 colour = "black", lwd = 1.2, alpha = .5) +
  scale_fill_manual("", values = c("dry" = "brown", "wet" = "green")) + 
  labs(x = "\nPlotID", y = expression("Average daily WUE (kgC/m"^"2"~")")) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        legend.position = c(0, 1), 
        legend.justification = c(0, .9)) + 
  coord_cartesian(ylim = num_ylim)

  
  