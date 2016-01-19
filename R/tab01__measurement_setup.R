## environmental stuff

# packages
library(foreach)
library(rgdal)

# functions
source("R/slsPlots.R")
source("R/slsAvlFls.R")
source("R/slsPathLength.R")
source("R/slsPathHeight.R")
source("R/slsPathInclination.R")

# path: coordinates
ch_dir_crd <- "../../kilimanjaro/coordinates/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

# sls workspace path and files (sub-directories)
ch_dir_ws <- "/media/permanent/SRun"
ch_fls_ws <- list.files(ch_dir_ws, pattern = "^workspace_SLS", 
                        full.names = TRUE)


## data

# sls plots incl. season
df_plt_ssn <- slsAvlFls()
df_plt_ssn <- df_plt_ssn[, c(1, 3)]
df_plt_ssn <- subset(df_plt_ssn, season == "r")

df_plt_ssn <- merge(data.frame(plot = rev(slsPlots(style = "elevation"))), 
                    df_plt_ssn, by = 1, all.x = TRUE, sort = FALSE)

# plot coordinates
spt_plt <- readOGR(ch_dir_crd, ch_fls_crd)
spt_plt <- subset(spt_plt, PoleType == "AMP")
spt_plt_sls <- subset(spt_plt, PlotID %in% slsPlots())
spt_plt_sls <- spt_plt_sls[, c("PlotID", "Z_DEM_HMP")]

# merge
df_plt_ssn_ele <- merge(df_plt_ssn, spt_plt_sls@data, by = 1, sort = FALSE)

# sampled research sites
ch_fls_ws <- ch_fls_ws[-grep("201409", ch_fls_ws)]
ls_plt <- strsplit(ch_fls_ws, "_")
ch_plt <- sapply(ls_plt, "[[", 3)
ch_plt <- as.character(df_plt_ssn[, 1])

# extraction of relevant information
id <- sapply(ch_plt, function(i) grep(i, ch_fls_ws))
ch_fls_ws  <- ch_fls_ws[id]

df_cfg <- foreach(i = ch_fls_ws, j = ch_plt, 
                  .combine = "rbind") %do% {
  num_pl <- slsPathLength(i)
  num_ph <- slsPathHeight(i)
  
  data.frame(plot = j, path_length = num_pl, path_height = num_ph)  
}
df_cfg$season <- df_plt_ssn$season

# shortest/longest path length/height
df_cfg[which.min(df_cfg$path_length), ]
df_cfg[which.max(df_cfg$path_length), ]

df_cfg[which.min(df_cfg$path_height), ]
df_cfg[which.max(df_cfg$path_height), ]

# path inclination
df_slp <- slsPathInclination()
df_slp$plot <- tolower(df_slp$plot)
df_slp$season <- df_plt_ssn$season

# merge path information
ls_all <- list(df_plt_ssn_ele, df_cfg, df_slp)
df_all <- Reduce(function(...) merge(..., by = c("plot", "season"), sort = FALSE), ls_all)
df_all$habitat <- substr(df_all$plot, 1, 3)

## rearrange columns
df_all <- df_all[, c(9, 1, 3, 6, 4, 5, 7)]
names(df_all) <- c("Habitat", "Plot", "Elevation (m a.s.l.)", "Inclination", 
                   "Path length (m)", "Path height (m)", "Beam slope")

df_all[, 3] <- round(df_all[, 3])
df_all[, 4] <- round(df_all[, 4], 1)
write.csv(df_all, "data/tab01.csv", quote = FALSE, row.names = TRUE)

stargazer(df_all, summary = FALSE, rownames = FALSE)
