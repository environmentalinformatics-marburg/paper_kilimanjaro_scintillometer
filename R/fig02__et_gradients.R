# packages
library(lubridate)
library(ggplot2)

# sls plots
ch_sls_plt <- c("sav0", "sav5", "mai0", "mai4", 
                "gra1", "gra2", "cof3", "cof2", 
                "fer0", "fed1", "hel1")

# 60-min data
ls_sls_dv_20m <- lapply(1:nrow(df_sls_fls_rs), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls_rs$mrg[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls_rs$plot[i], habitat = df_sls_fls_rs$habitat[i],
             season = df_sls_fls_rs$season[i], tmp_df)
})
df_sls_dv_20m <- do.call("rbind", ls_sls_dv_20m)

ch_sls_dv_20m_hr <- substr(as.character(df_sls_dv_20m$datetime), 1, 2)
int_sls_dv_20m_hr <- as.integer(ch_sls_dv_20m_hr)
int_sls_dv_20m_dt <- int_sls_dv_20m_hr >= 4 & int_sls_dv_20m_hr < 20
df_sls_dv_20m_dt <- df_sls_dv_20m[int_sls_dv_20m_dt, ]

df_sls_dv_20m_dt$time <- strptime(df_sls_dv_20m_dt$datetime, format = "%H:%M:%S")
df_sls_dv_20m_dt$time_fac <- factor(format(df_sls_dv_20m_dt$time, format = "%H:%M"))

# reorder habitat factor levels
df_sls_dv_20m_dt$habitat_new <- factor(df_sls_dv_20m_dt$habitat, 
                                       levels = rev(c("mai", "sav", "cof", "gra", 
                                                      "fed", "fer", " ", "hel")))

# x-axis labels
ch_lvl <- levels(df_sls_dv_20m_dt$time_fac)
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
       data = df_sls_dv_20m_dt) + 
  geom_histogram(stat = "identity", position = "dodge") +
  facet_wrap(~ habitat_new, ncol = 2, drop = FALSE) + 
  scale_x_discrete(labels = ch_lbl) + 
  scale_color_manual("", values = ch_cols_bg) + 
  scale_fill_manual("", values = ch_cols_bg) + 
  guides(colour = FALSE, fill = FALSE) + 
  labs(x = "\nTime (hours)", y = "Evapotranspiration\n") + 
  theme_bw()

###

g = ggplotGrob(p)
## remove empty panels
g$grobs[names(g$grobs) %in% c("panel1", "panel2", "strip_t.1", "strip_t.2")] = NULL
## remove them from the layout
g$layout = g$layout[!(g$layout$name %in% c("panel-1", "panel-2", 
                                           "strip_t-1", "strip_t-2")),]
## move axis closer to panel
g$layout[g$layout$name == "axis_l-1", c("l", "r")] = c(9,9)
grid.newpage()
grid.draw(g)
