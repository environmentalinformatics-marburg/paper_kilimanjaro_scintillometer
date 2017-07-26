### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
lib <- c("raster", "rgdal", "foreach", "stargazer")
Orcs::loadPkgs(lib)

## functions
source("R/slsFcts.R")


### processing -----

## sls workspace path and files (sub-directories)
dir_ws <- "/media/fdetsch/Permanent/SRun"
fls_ws <- list.files(dir_ws, pattern = "^workspace_SLS", 
                        full.names = TRUE)

## sls plots incl. season
df_plt_ssn <- slsAvlFls()
df_plt_ssn <- slsTemporalRange(df_plt_ssn)
df_plt_ssn <- merge(data.frame(PlotID = rev(slsPlots(style = "elevation"))), 
                    df_plt_ssn, by = 1, all.x = TRUE, sort = FALSE)

## plot coordinates
spt_plt <- readOGR("../../data/kili/coordinates", 
                   "PlotPoles_ARC1960_mod_20140807_final", 
                   p4s = "+init=epsg:21037")
spt_plt_sls <- subset(spt_plt, PlotID %in% slsPlots() & PoleType == "AMP")
spt_plt_sls <- spt_plt_sls[, c("PlotID", "Z_DEM_HMP")]

## aspect
dem <- raster("../../kilimanjaro/coordinates/DEM_ARC1960_30m_Hemp.tif")
asp <- terrain(dem, opt = "aspect", unit = "degrees")
spt_plt_sls@data$ASP_DEM_HMP <- round(extract(asp, spt_plt_sls))

## merge
df_plt_ssn_ele <- merge(df_plt_ssn, spt_plt_sls@data, by = 1, sort = FALSE)

## sampled research sites
# fls_ws <- fls_ws[-grep("201409", fls_ws)]
# ls_plt <- strsplit(fls_ws, "_")
# plt <- sapply(ls_plt, "[[", 3)
# plt <- as.character(df_plt_ssn[, 1])
# 
# # extraction of relevant information
# id <- sapply(plt, function(i) grep(i, fls_ws))
# fls_ws  <- fls_ws[id]

id <- unlist(sapply(rev(slsPlots(style = "elevation")), function(i) {
  grep(i, fls_ws)
}))
fls_ws <- fls_ws[id]

df_cfg <- foreach(i = fls_ws, j = df_plt_ssn_ele$plot, 
                  .combine = "rbind") %do% {
  num_pl <- slsPathLength(i)
  num_ph <- slsPathHeight(i)
  
  data.frame(PlotID = j, path_length = num_pl, path_height = num_ph)  
}
df_cfg$season <- df_plt_ssn$season

# shortest/longest path length/height
df_cfg[which.min(df_cfg$path_length), ]
df_cfg[which.max(df_cfg$path_length), ]

df_cfg[which.min(df_cfg$path_height), ]
df_cfg[which.max(df_cfg$path_height), ]

# path inclination
df_slp <- slsPathInclination()

# merge path information
ls_all <- list(df_plt_ssn_ele, df_cfg, df_slp)
df_all <- Reduce(function(...) merge(..., by = c("PlotID", "season"), 
                                     sort = FALSE), ls_all)

## canopy heights
heights <- c(10, NA, 1.5, NA, NA, 200, 40, 
             120, 180, 400, '-\"-', 460, '-\"-')
df_all$heights <- heights

## rearrange rows, remove column 'season'
df_all$PlotID <- as.character(df_all$PlotID)
df_all$PlotID[df_all$season == "d"] <- 
  paste(df_all$PlotID[df_all$season == "d"], "(d)")

id <- rev(match(slsPlots("elevation", dry = TRUE), df_all$PlotID))
df_all <- df_all[id, ]
df_all <- df_all[, -2]

## rearrange columns
df_all <- df_all[, c(1:4, 8, 5:7, 9)]
df_all[, 4] <- round(df_all[, 4])

## reformat dates
df_all[, c("begin", "end")] <- format(df_all[, c("begin", "end")], "%Y%j")

## write to file and create output table
write.csv(df_all, "data/tab01.csv", quote = FALSE, row.names = TRUE)

stargazer(df_all, summary = FALSE, rownames = FALSE, digits = NA, 
          align = TRUE)
