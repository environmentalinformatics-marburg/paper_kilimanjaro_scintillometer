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
df_sls_fls_rs <- slsAvlFls(ssn = "r", disturbed = TRUE)

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
  select(PlotID, Time, ETmu) %>%
  group_by(PlotID) %>% 
  filter(ETmu == max(ETmu)) %>%
  mutate(ETmax = round(ETmu, 2)) %>%
  select(PlotID, ETmax, Time) %>%
  data.frame() %>%
  arrange(desc(ETmax)) -> df_max_hr

df_all %>%
  select(PlotID, Time, ETmu) %>%
  group_by(PlotID) %>% 
  summarise(sumETmu = round(sum(ETmu, na.rm = TRUE), 2)) %>%
  data.frame() %>%
  arrange(desc(sumETmu)) -> df_max_dy

df_max <- merge(df_max_dy, df_max_hr, by = "PlotID", sort = FALSE)
write.csv(df_max, "../../phd/scintillometer/data/tbl/table_3|2.csv")

## daytime subset
ch_all_hr <- substr(df_all$Time, 1, 2)
int_all_hr <- as.integer(ch_all_hr)
int_all_dt <- int_all_hr >= 4 & int_all_hr < 20
df_all <- df_all[int_all_dt, ]

df_all$Time <- strptime(df_all$Time, format = "%H:%M:%S")
df_all$Time <- factor(format(df_all$Time, format = "%H:%M"))

## reorder factor levels
df_all$PlotID <- factor(df_all$PlotID, 
                        levels = rev(c("mai4", "mai0", "cof2", "cof3", "", "fed1")))

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
ch_lvl_min <- sapply(ls_lvl, "[[", 2)

int_lvl_hr_odd_full <- ch_lvl_min == "00" & int_lvl_hr_odd
ch_lbl[int_lvl_hr_odd_full] <- ch_lvl[int_lvl_hr_odd_full]
names(ch_lbl) <- ch_lvl

## visualization
p <- ggplot(aes(x = Time, y = ETmu), data = df_all) + 
  geom_histogram(stat = "identity", fill = "grey65", colour = "grey35") + 
  geom_errorbar(limits, position = "dodge", width = .2) + 
  geom_hline(aes(yintercept = 0), colour = "grey50", linetype = "dashed") + 
  facet_wrap(~ PlotID, ncol = 2, drop = FALSE) + 
  scale_x_discrete(labels = ch_lbl) + 
  labs(x = "\nTime (hours)", y = "Evapotranspiration (mm)\n") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        text = element_text(size = 14), axis.text = element_text(size = 10))

png(paste0(ch_dir_ppr, "/fig/fig04__disturbance_gradient.png"), width = 22.5, 
    height = 20, units = "cm", res = 300, pointsize = 18)
grid.newpage()
vp1 <- viewport(x = 0, y = 0, width = 1, height = 1,
                just = c("left", "bottom"), name = "p")
pushViewport(vp1)
current.viewport()
print(p, newpage = FALSE)
vp2 <- viewport(x = .535, y = .68, width = .45, height = .3, 
                just = c("left", "bottom"), name = "p2")
pushViewport(vp2)
current.viewport()
grid.rect(gp = gpar(col = "white"))
dev.off()
