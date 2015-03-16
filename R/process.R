## Environmental stuff

# packages
lib <- c("randomForest", "ggplot2", "latticeExtra", "foreach")
sapply(lib, function(x) stopifnot(require(x, character.only = TRUE)))

# functions
source("R/slsMergeDailyData.R")
source("R/slsProcessing.R")
source("R/slsDiurnalVariation.R")
source("R/slsAggregate.R")

# path: srun
ch_dir_srun <- "../../SRun/"

# path: output storage 
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"

# relevant files
ch_pattern <- c("mrg.csv$", "mrg_rf.csv$", "mrg_rf_agg01h.csv$")

ls_df_sls_fls <- foreach(tmp_ch_pattern = ch_pattern) %do% {
  tmp_ch_fls <- list.files("../../phd/scintillometer/data", full.names = TRUE,
                           pattern = tmp_ch_pattern, recursive = TRUE)
  
  # current plot
  ch_sls_plt <- basename(tmp_ch_fls)
  ls_sls_plt <- strsplit(ch_sls_plt, "_")
  ch_sls_plt <- sapply(ls_sls_plt, "[[", 1)
  
  ch_sls_hab <- substr(ch_sls_plt, 1, 3)
  
  # current season
  int_sls_ds <- which(duplicated(ch_sls_plt))
  ch_sls_ssn <- rep("r", length(ch_sls_plt))
  ch_sls_ssn[int_sls_ds] <- "d"
  
  ch_process_level <- substr(tmp_ch_pattern, 1, nchar(tmp_ch_pattern)-5)
  tmp_df_fls <- data.frame(ch_sls_plt, ch_sls_hab, ch_sls_ssn, tmp_ch_fls, 
                           stringsAsFactors = FALSE)
  names(tmp_df_fls) <- c("plot", "habitat", "season", ch_process_level)
  return(tmp_df_fls)
}
df_sls_fls <- Reduce(function(...) merge(..., by = c(1, 2, 3), sort = FALSE), 
                     ls_df_sls_fls)

# subset rainy season measurements
df_sls_fls_rs <- subset(df_sls_fls, season == "r")


## data

srunWorkspaces <- dir(ch_dir_srun, pattern = "workspace_SLS", recursive = FALSE, 
                      full.names = TRUE)

# gap-filling and 1h aggregation
ls_sls <- lapply(srunWorkspaces, function(i) {
  slsProcessing(i, dsn = "../../phd/scintillometer/data/sls")
})

ch_sls_1h <- list.files("../../phd/scintillometer/data", full.names = TRUE,
                        pattern = "_mrg_rf_agg01h.csv$", recursive = TRUE)

ls_sls_1h <- foreach(i = df_sls_fls$mrg_rf_agg01h, j = df_sls_fls$season, 
                     k = df_sls_fls$plot) %do% {
                       tmp_df <- read.csv(i)
                       return(data.frame(plot = k, season = j, tmp_df))
                     }
df_sls_1h <- do.call("rbind", ls_sls_1h)
df_sls_1h$datetime <- strptime(df_sls_1h$datetime, format = "%Y-%m-%d %H:%M")

# plot colors
cols <- c("sav0" = "darkgoldenrod", "mai0" = "darkolivegreen", 
          "sav5" = "darkgoldenrod", "mai4" = "darkolivegreen", 
          "gra1" = "chartreuse", "cof3" = "burlywood4", 
          "gra2" = "chartreuse", "cof2" = "burlywood4")

# plot names
ch_sls_plt <- c("sav0", "sav5", "mai0", "mai4", 
                "gra1", "gra2", "cof3", "cof2", 
                "fer0", "fed1", "hel1")

# ggplot(aes(x = datetime, y = waterET), data = df_sls_1h) + 
#   geom_histogram(stat = "identity") + 
#   facet_wrap(~ plot + season, ncol = 2, scales = "free_x") + 
#   geom_hline(aes(y = 0), colour = "darkgrey") + 
#   labs(x = "Time [h]", y = "Evapotranspiration [mm/h]") + 
#   theme_bw()

# Compare boxplot time series of corresponding natural and disturbed LUCs
ls_sls_dv_20m <- lapply(1:nrow(df_sls_fls), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls$mrg[i], agg_by = 20, 
                                FUN = function(...) median(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df)
})

df_sls_dv_20m <- do.call("rbind", ls_sls_dv_20m)

# df_sls_dv_20m_rs <- subset(df_sls_dv_20m, season == "r")
# 
# df_sls_dv_20m_rs$facet <- "SAV0 vs. MAI0"
# df_sls_dv_20m_rs$facet[df_sls_dv_20m_rs$plot %in% c("sav5", "mai4")] <- "SAV5 vs. MAI4"
# df_sls_dv_20m_rs$facet[df_sls_dv_20m_rs$plot %in% c("gra1", "cof3")] <- "GRA1 vs. COF3"
# df_sls_dv_20m_rs$facet[df_sls_dv_20m_rs$plot %in% c("gra2", "cof2")] <- "GRA2 vs. COF2"
# df_sls_dv_20m_rs$facet[df_sls_dv_20m_rs$plot %in% c("fer0", "fed1")] <- "FER0 vs. FED1"
# df_sls_dv_20m_rs$facet <- factor(df_sls_dv_20m_rs$facet, 
#                                  levels = c("SAV0 vs. MAI0", "SAV5 vs. MAI4", 
#                                             "GRA1 vs. COF3", "GRA2 vs. COF2", 
#                                             "FER0 vs. FED1"))
# 
# df_sls_dv_20m_rs$luc <- factor(substr(df_sls_dv_20m_rs$plot, 1, 3), 
#                                levels = c("sav", "mai", "gra", "cof", "fer", "fed"))

# ggplot(aes(x = time, y = waterET, fill = luc), data = df_sls_dv_20m_rs) + 
#   geom_boxplot(outlier.colour = NA, notch = FALSE) + 
#   facet_wrap(~ facet) +
#   scale_fill_manual("Land-cover type", 
#                     values = c("darkgoldenrod", "darkolivegreen", 
#                                "chartreuse", "burlywood4")) +
#   ylim(-.05, 1) +
#   labs(x = "Hour of day", y = "Evapotranspiration [mm/h]") + 
#   theme_bw()

# # 20-min aggregation                 
# ls_sls_dv_20m <- lapply(ch_sls_plt, function(i) {
#   tmp_int_id <- grep(i, df_sls_fls_rs$plot)
#   tmp_df <- slsAggregate(fn = df_sls_fls_rs$mrg[tmp_int_id], agg_by = 20, 
#                          FUN = function(...) median(..., na.rm = TRUE))
#   data.frame(plotid = i, tmp_df, season = df_sls_fls_rs$season[tmp_int_id])
# })
# # time stamp to factor
# df_sls_dv_20m <- do.call("rbind", ls_sls_dv_20m)
# 
# int_sls_dv_20m_hr <- hour(df_sls_dv_20m$datetime)
# int_sls_dv_20m_dt <- int_sls_dv_20m_hr >= 4 & int_sls_dv_20m_hr < 20
# df_sls_dv_20m_dt <- df_sls_dv_20m[int_sls_dv_20m_dt, ]
# 
# df_sls_dv_20m_dt$time <- strptime(df_sls_dv_20m_dt$time, format = "%H:%M:%S")
# df_sls_dv_20m_dt$time_fac <- factor(format(df_sls_dv_20m_dt$time, format = "%H:%M"))
# 
# # x-axis labels
# ch_lvl <- levels(df_sls_dv_20m_dt$time_fac)
# ch_lbl <- rep("", length(ch_lvl))
# 
# ls_lvl <- strsplit(ch_lvl, ":")
# ch_lvl_hr <- sapply(ls_lvl, "[[", 1)
# int_lvl_hr <- as.integer(ch_lvl_hr)
# int_lvl_hr_odd <- int_lvl_hr %% 2 != 0
# ch_lvl_min <- sapply(ls_lvl, "[[", 2)
# 
# int_lvl_hr_odd_full <- ch_lvl_min == "00" & int_lvl_hr_odd
# ch_lbl[int_lvl_hr_odd_full] <- ch_lvl[int_lvl_hr_odd_full]
# names(ch_lbl) <- ch_lvl

# # head-to-head comparison
# foreach(h = list(c("sav0", "mai0"), c("sav5", "mai4"), 
#                  c("gra1", "cof3"), c("gra2", "cof2"), 
#                  c("fer0", "fed1"), "hel1")) %do% {
#   
#   # 20-min aggregation                 
#   tmp_ls_sls_dv_20m <- lapply(h, function(i) {
#     tmp_int_id <- grep(i, df_sls_fls_rs$plot)
#     tmp_df <- slsAggregate(fn = df_sls_fls_rs$mrg[tmp_int_id], agg_by = 20)
#     data.frame(plotid = i, tmp_df, season = df_sls_fls_rs$season[tmp_int_id])
#   })
# 
#   # time stamp to factor
#   tmp_df_sls_dv_20m <- do.call("rbind", tmp_ls_sls_dv_20m)
#   tmp_df_sls_dv_20m$time <- strptime(tmp_df_sls_dv_20m$time, format = "%H:%M:%S")
#   tmp_df_sls_dv_20m$time_fac <- factor(format(tmp_df_sls_dv_20m$time, 
#                                               format = "%H:%M"))
#   
#   # vis
#   ggplot(aes(x = time_fac, y = waterET, fill = plotid), 
#          data = tmp_df_sls_dv_20m) + 
#     geom_boxplot(outlier.colour = NA, notch = FALSE) + 
#     scale_fill_manual("", values = cols) +
#     scale_x_discrete(labels = ch_lbl) + 
#     ylim(-.05, 1) +
#     labs(x = "\nTime (20 min)", y = "Evapotranspiration (mm/h)\n") + 
#     theme_bw() + 
#     theme(legend.position = c(0, 1), 
#           legend.justification = c(0, 1), 
#           legend.direction = "horizontal")
# }

# vis
ggplot(aes(x = time_fac, y = waterET), data = df_sls_dv_20m_dt) + 
  geom_boxplot(outlier.colour = NA, notch = FALSE, colour = "grey65") + 
  stat_smooth(aes(x = as.numeric(time_fac), y = waterET), data = df_sls_dv_20m_dt, 
              method = "loess", span = .3, colour = "black") +
  facet_wrap(~ plotid, ncol = 2) + 
  scale_x_discrete(labels = ch_lbl) + 
  ylim(-.05, 1) +
  labs(x = "\nTime (20 min)", y = "Evapotranspiration (mm/h)\n") + 
  theme_bw() + 
  theme(panel.grid = element_blank())

# hourly aggregation                 
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls$mrg[i], agg_by = 60, 
                                FUN = function(...) median(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df)
})
df_sls_dv_01h <- do.call("rbind", ls_sls_dv_01h)

# diurnal aggregation
ls_sls_dv_01d <- lapply(ls_sls_dv_01h, function(i) {
  tmp.df <- slsAggregate(fn = i, agg_by = 24, include_time = FALSE,
                         FUN = function(...) sum(..., na.rm = TRUE))
  data.frame(plotid = unique(i$plot), season = unique(i$season), tmp.df)
})
df_sls_dv_01d <- do.call("rbind", ls_sls_dv_01d)
save("df_sls_dv_01d", file = paste0(ch_dir_out_agg01d, "df_sls_dv_01d.RData"))
