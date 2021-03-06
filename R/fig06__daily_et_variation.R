### environmental stuff --------------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

# output path
ch_dir_os <- switch(Sys.info()[["sysname"]], 
                    "Linux" = "/media/permanent/", 
                    "Windows" = "C:/Permanent/")

ch_dir_ppr <- paste0(ch_dir_os, 
                     "publications/paper/detsch_et_al__spotty_evapotranspiration/")

# sls plots and referring files
ch_sls_plt <- slsPlots()
df_sls_fls_rs <- slsAvlFls(ssn = "r")

## hourly aggregation
ls_all <- lapply(df_sls_fls_rs[, "mrg_rf_agg01h"], function(i) {
  tmp_ch_plot <- substr(basename(i), 1, 4)
  
  ## import, reformat and merge daily data
  tmp_df <- read.csv(i, stringsAsFactors = FALSE)
  tmp_df <- tmp_df[, c("datetime", "waterET")]
  tmp_df[, "Time"] <- sapply(strsplit(tmp_df[, "datetime"], " "), "[[", 2)
  tmp_ls <- split(tmp_df, yday(tmp_df$datetime))
  tmp_df_mrg <- Reduce(function(...) merge(..., by = "Time"), tmp_ls)
  tmp_df_mrg <- tmp_df_mrg[, -grep("datetime", names(tmp_df_mrg))]
  
  ## hourly means (mu) and standard errors (se)
  tmp_num_mu <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) mean(..., na.rm = TRUE))
  tmp_num_se <- apply(tmp_df_mrg[, 2:ncol(tmp_df_mrg)], 1, 
                      FUN = function(...) std.error(..., na.rm = TRUE))

  ## merge and return relevant data
  tmp_df_all <- data.frame(PlotID = tmp_ch_plot, Time = tmp_df_mrg[, "Time"], 
                           ETmu = tmp_num_mu, ETse = tmp_num_se)
  return(tmp_df_all)
})
df_all <- do.call("rbind", ls_all)

## maximum hourly and daily et rates per plot (table 3)
df_all %>%
  dplyr::select(PlotID, Time, ETmu) %>%
  group_by(PlotID) %>% 
  filter(ETmu == max(ETmu)) %>%
  mutate(ETmax = round(ETmu, 2)) %>%
  dplyr::select(PlotID, ETmax, Time) %>%
  data.frame() %>%
  arrange(desc(ETmax)) -> df_max_hr

df_all %>%
  dplyr::select(PlotID, Time, ETmu) %>%
  group_by(PlotID) %>% 
  summarise(sumETmu = round(sum(ETmu, na.rm = TRUE), 2)) %>%
  data.frame() %>%
  arrange(desc(sumETmu)) -> df_max_dy

df_max <- merge(df_max_dy, df_max_hr, by = "PlotID", sort = FALSE)
write.csv(df_max, "../../phd/scintillometer/data/tbl/table_3.csv")

## daytime subset
ch_all_hr <- substr(df_all$Time, 1, 2)
int_all_hr <- as.integer(ch_all_hr)
int_all_dt <- int_all_hr >= 4 & int_all_hr < 20
df_all <- df_all[int_all_dt, ]

df_all$Time <- strptime(df_all$Time, format = "%H:%M:%S")
df_all$Time <- factor(format(df_all$Time, format = "%H"))

## reorder factor levels
df_all$PlotID <- factor(df_all$PlotID, 
                        levels = rev(c("mai0", "mai4", "sav0", "sav5", 
                                       "gra2", "gra1", "cof2", "cof3", 
                                       "", "fer0", "hel1", "fed1")))

## ylim adjustment
limits <- aes(ymax = ETmu + ETse, ymin = ETmu - ETse)
num_ylim <- c(min(df_all$ETmu - df_all$ETse, na.rm = TRUE) - .02, 
              max(df_all$ETmu + df_all$ETse, na.rm = TRUE) + .02)

## x-axis labels
ch_lvl <- levels(df_all$Time)
ch_lbl <- rep("", length(ch_lvl))

ls_lvl <- strsplit(ch_lvl, ":")
ch_lvl_hr <- sapply(ls_lvl, "[[", 1)
int_lvl_hr <- as.integer(ch_lvl_hr)
int_lvl_hr_odd <- int_lvl_hr %% 2 != 0
ch_lbl[int_lvl_hr_odd] <- ch_lvl[int_lvl_hr_odd]
names(ch_lbl) <- ch_lvl

## visualization
detach("package:Rsenal", unload = TRUE)
detach("package:OpenStreetMap", unload = TRUE)
detach("package:caret", unload = TRUE)
detach("package:ggplot2", unload = TRUE)
install.packages("inst/packages/ggplot2_1.0.1.tar.gz", repos = NULL)
library(ggplot2)

p <- ggplot(aes(x = Time, y = ETmu), data = df_all) + 
  geom_histogram(stat = "identity", fill = "grey80", colour = "grey35", 
                 size = .5) + 
  geom_errorbar(limits, position = "dodge", width = .2) +
  stat_smooth(aes(group = 1), method = "loess", se = FALSE, colour = "red", 
              span = .99) + 
  geom_hline(aes(yintercept = 0), colour = "grey50", linetype = "dashed") + 
  facet_wrap(~ PlotID, ncol = 4, drop = FALSE) + 
  scale_x_discrete(labels = ch_lbl) + 
  labs(x = "\nHour of day", y = "ET (mm)\n") + 
  expand_limits(x = 0, y = 0) + 
  scale_y_continuous(limits = c(0, .775), expand = c(0, 0)) + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        text = element_text(size = 10), 
        axis.text = element_text(size = 8))

## in-text png version
png(paste0(ch_dir_ppr, "/fig/figure06.png"), width = 20, height = 12, 
    units = "cm", res = 500)
grid.newpage()
vp1 <- viewport(x = 0, y = 0, width = 1, height = 1,
                just = c("left", "bottom"), name = "p")
pushViewport(vp1)
print(p, newpage = FALSE)
vp2 <- viewport(x = .755, y = .68, width = .245, height = .32, 
                just = c("left", "bottom"), name = "p2")
pushViewport(vp2)
grid.rect(gp = gpar(col = "white"))
dev.off()

## standalone tiff version
setEPS()
postscript(paste0(ch_dir_ppr, "fig/figure06.eps"), width = 20*.3937, 
           height = 12*.3937)
grid.newpage()
vp1 <- viewport(x = 0, y = 0, width = 1, height = 1,
                just = c("left", "bottom"), name = "p")
pushViewport(vp1)
print(p, newpage = FALSE)
vp2 <- viewport(x = .755, y = .68, width = .245, height = .32, 
                just = c("left", "bottom"), name = "p2")
pushViewport(vp2)
grid.rect(gp = gpar(col = "white", fill = "white"))
dev.off()
