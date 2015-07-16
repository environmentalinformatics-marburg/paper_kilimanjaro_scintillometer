### environmental stuff

## packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# ## modis settings
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)

## path: modis, processed
ch_dir_mod15 <- "/media/permanent/phd/scintillometer/data/mod15a2/"
ch_dir_myd15 <- "/media/permanent/phd/scintillometer/data/myd15a2/"

## path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

## path: output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"


### processing

## data: sls measurements
df_sls_fls <- slsAvlFls()

## data: coordinates
spt_plt <- readOGR(ch_dir_crd, ch_fls_crd, p4s = "+init=epsg:21037")
spt_plt <- subset(spt_plt, PoleType == "AMP")
spt_plt <- subset(spt_plt, PlotID %in% slsPlots())

## data: lai-2200
ch_fls_lai <- list.files("../../phd/lai-2200", pattern = ".TXT$", 
                         full.names = TRUE)

## sampled research sites
ch_plt <- basename(ch_fls_lai)
ls_plt <- strsplit(ch_plt, "\\.")
ch_plt <- sapply(ls_plt, "[[", 1)

## import lai-2200 datasets
ls_lai <- lapply(ch_fls_lai, function(i) {
  tmp_ch_rl <- readLines(i)
  
  # date
  tmp_int_id_dt <- grep("DATE", tmp_ch_rl)
  tmp_ch_dt <- tmp_ch_rl[tmp_int_id_dt]
  tmp_ls_dt <- strsplit(tmp_ch_dt, "\t")
  tmp_ch_dt <- sapply(tmp_ls_dt, "[[", 2)
  tmp_ch_dt <- substr(tmp_ch_dt, 1, 8)
  tmp_dt <- as.Date(tmp_ch_dt, format = "%Y%m%d")
  
  # lai
  tmp_int_id_lai <- grep("LAI\t", tmp_ch_rl)
  tmp_ch_lai <- tmp_ch_rl[tmp_int_id_lai]
  tmp_ls_lai <- strsplit(tmp_ch_lai, "\t")
  tmp_ch_lai <- sapply(tmp_ls_lai, "[[", 2)
  tmp_num_lai <- as.numeric(tmp_ch_lai)
  
  tmp_df <- data.frame(Date = tmp_dt, LAI = tmp_num_lai)
  return(tmp_df)
})
df_lai <- do.call("rbind", ls_lai)

# merge
df_plt_lai <- data.frame(plot = ch_plt, df_lai)

df_plt_lai$season <- "r"
int_id_dryssn <- grep("-D", df_plt_lai$plot)
df_plt_lai$season[int_id_dryssn] <- "d"
df_plt_lai$plot <- tolower(df_plt_lai$plot)
df_plt_lai$plot <- substr(df_plt_lai$plot, 1, 4)

save("df_plt_lai", file = "data/licor_lai.rds")


## data: modis

for (product in c("MOD15A2", "MYD15A2"))
  runGdal(product, begin = "2013001", tileH = 21, tileV = 9, 
          SDSstring = "011100", job = "LAI1km", outProj = "+init=epsg:21037")

# modis dates
ch_fls_lai <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                         pattern = paste0("^MYD15A2.*Lai_1km.tif$"))
ls_fls_lai <- extractDate(ch_fls_lai, asDate = TRUE)
dt_fls_lai <- ls_fls_lai$inputLayerDates

# extent
rst_tmp <- kiliAerial(minNumTiles = 9L, rasterize = TRUE)

# data import and cropping
ls_lai_crp <- foreach(sensor = c("MOD15A2", "MYD15A2"), 
                      path_out = c(ch_dir_mod15, ch_dir_myd15)) %do% {
  ls_myd17_crp <- foreach(sds = c("\\.Lai_", "FparLai", "FparExtra"), 
                          .packages = c("raster", "rgdal", "foreach")) %dopar% {
    tmp_ch_fls <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                             pattern = paste(sensor, sds, ".tif$", sep = ".*"))
    
    tmp_rst_crp <- foreach(i = tmp_ch_fls, .combine = "stack") %do% {
      tmp_rst <- raster(i)
      
      # application of scale factor
      if (sds == "\\.Lai_")
        tmp_rst <- tmp_rst * 0.1
      
      # crop by kili extent
      crop(tmp_rst, rst_tmp, format = "GTiff", overwrite = TRUE,
           filename = paste0(path_out, "/crp/CRP_", basename(i)))
    }
  
    return(tmp_rst_crp)
  }
}

rst_lai <- sapply(ls_lai_crp, "[[", 1)
rst_qc <- sapply(ls_lai_crp, "[[", 2)
rst_xqc <- sapply(ls_lai_crp, "[[", 3)

## quality check
ls_lai_crp_qc <- foreach(lai = rst_lai, qc = rst_qc, 
                         path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
  foreach(i = 1:nlayers(lai), j = names(lai),
          .packages = c("rgdal", "raster"), .combine = "stack") %dopar% {
    overlay(lai[[i]], qc[[i]], fun = function(x, y) {
      num_x <- x[]
      num_y <- y[]
      num_y_qc <- sapply(num_y, function(z) qcMyd17(z, qc = TRUE, dd = TRUE))
      log_y_qc <- is.na(num_y_qc)
      num_x[log_y_qc] <- NA
      return(num_x)
    }, filename = paste0(path_out, "/qc/QC_", j), format = "GTiff", 
    overwrite = TRUE)
  }
}

ls_lai_crp_qc <- foreach(path_in = list(ch_dir_mod15, ch_dir_myd15)) %do% {
  fls_lai_crp_qcx <- list.files(path_in, pattern = "^QC_", 
                                full.names = TRUE, recursive = TRUE)
  stack(fls_lai_crp_qcx)
}

## fill single gaps based on focal median
ls_lai_crp_qc_md <- foreach(lai = ls_lai_crp_qc, 
        path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
  foreach(i = 1:nlayers(lai), j = names(lai), 
          .combine = "stack", .packages = c("raster", "rgdal")) %do% {
            
            # identify gaps in current layer
            tmp_rst <- lai[[i]]
            tmp_mat <- as.matrix(tmp_rst)
            num_val <- tmp_rst[]
            int_na <- which(is.na(num_val))
            
            # fill each gap with focal median
            ls_adj <- madjacent(tmp_mat, int_na, directions = 8)
            num_val_md <- sapply(1:length(int_na), function(k) {
              int_id <- int_na[k]
              int_adj <- ls_adj[[k]]
              
              if (sum(is.na(num_val[int_adj])) > 2) {
                return(NA)
              } else {
                return(median(num_val[int_adj], na.rm = TRUE))
              }
            })
                        
            tmp_rst[int_na] <- num_val_md
            
            # save and return raster
            tmp_rst <- 
              writeRaster(tmp_rst, 
                          filename = paste0(path_out, "/md/MD_", j), 
                          format = "GTiff", overwrite = TRUE)
            return(tmp_rst)
  }
}

# ## extra quality check (not recommended)
# ls_lai_crp_qc_qcx <- foreach(lai = ls_lai_crp_qc, xqc = rst_xqc, 
#                              path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
#   foreach(i = 1:nlayers(lai), j = names(lai), 
#           .packages = c("rgdal", "raster"), .combine = "stack") %dopar% {
#     overlay(lai[[i]], xqc[[i]], fun = function(x, y) {
#       num_x <- x[]
#       num_y <- y[]
#       num_y_qc <- sapply(num_y, qcxMyd15)
#       log_y_qc <- is.na(num_y_qc)
#       num_x[log_y_qc] <- NA
#       return(num_x)
#     }, filename = paste0(path_out, "/qcx/QCX_", j), format = "GTiff", 
#     overwrite = TRUE)
#   }
# }
# 
# ls_lai_crp_qcx <- foreach(path_in = list(ch_dir_mod15, ch_dir_myd15)) %do% {
#   fls_lai_crp_qcx <- list.files(path_in, pattern = "^QCX_", 
#                                 full.names = TRUE, recursive = TRUE)
#   stack(fls_lai_crp_qcx)
# }
  
# remove pixels neighboring clouds
ls_lai_crp_qc_md_adj <- foreach(i = ls_lai_crp_qc_md, 
                                path_out = list(ch_dir_mod15, ch_dir_myd15)) %do% {
                                 
  tmp_ls <- unstack(i)
  
  # remove adjacent pixels
  tmp_ls_adj <- foreach(j = tmp_ls, .packages = lib) %dopar% {
    cells <- which(is.na(j[]))
    id <- adjacent(j, cells = cells, directions = 4, pairs = FALSE)
    j[id] <- NA
    return(j)
  }
  tmp_rst_adj <- stack(tmp_ls_adj)
  
  # save images
  tmp_ch_fls_out <- paste0(path_out, "/adj/ADJ_", names(i))
  tmp_ls_adj <- foreach(j = tmp_ch_fls_out, k = 1:length(tmp_ch_fls_out)) %do%
    writeRaster(tmp_rst_adj[[k]], filename = j, format = "GTiff", overwrite = TRUE)
  tmp_rst_adj <- stack(tmp_ls_adj)
  
  return(tmp_rst_adj)
}

# merge qc data
rst_lai_crp_qc_md_adj_mrg <- 
  foreach(lai_terra = unstack(ls_lai_crp_qc_md_adj[[1]]), 
          lai_aqua = unstack(ls_lai_crp_qc_md_adj[[2]]), 
          .combine = "stack", .packages = lib) %dopar% {
            overlay(lai_terra, lai_aqua, fun = function(x, y) {
              log_isna_x <- is.na(x)
              log_isna_y <- is.na(y)
              log_isna <- cbind(log_isna_x, log_isna_y)
              
              num_lai_mrg <- sapply(1:nrow(log_isna), function(i) {
                if (all(log_isna[i, ])) {
                  return(NA)
                } else if (all(!log_isna[i, ])) {
                  tmp_mu <- max(x[i], y[i])
                  return(tmp_mu)
                } else if (!log_isna_x[i] & log_isna_y[i]) {
                  return(x[i])
                } else {
                  return(y[i])
                }
              })
              return(num_lai_mrg)
            }, filename = paste0(ch_dir_myd15, "/mrg/MRG_", names(lai_aqua)), 
            format = "GTiff", overwrite = TRUE)
          }

fls_lai_crp_qc_md_adj_mrg <- list.files(ch_dir_myd15, pattern = "^MRG_ADJ_", 
                                 full.names = TRUE, recursive = TRUE)
rst_lai_crp_qc_md_adj_mrg <- stack(fls_lai_crp_qc_md_adj_mrg)
mat_lai_crp_qc_md_adj_mrg <- as.matrix(rst_lai_crp_qc_md_adj_mrg)



## gap-filling (kza(..., k = 1) is equal to moving average)
ls_kz <- lapply(1:nrow(mat_lai_crp_qc_md_adj_mrg), function(i) {
  num_val <- mat_lai_crp_qc_md_adj_mrg[i, ]
  int_na <- which(is.na(mat_lai_crp_qc_md_adj_mrg[i, ]))
  
  if (length(int_na) > 0) {
    tmp_num_kz <- kza(mat_lai_crp_qc_md_adj_mrg[i, ], m = 3, k = 3, 
                      impute_tails = TRUE)$kz
    num_val[int_na] <- tmp_num_kz[int_na]
  }
  
  return(num_val)
})
mat_kz <- do.call("rbind", ls_kz)
rst_kz <- rst_lai_crp_qc_md_adj_mrg
rst_kz <- setValues(rst_kz, mat_kz)
rst_kz <- writeRaster(rst_kz, filename = paste0(ch_dir_myd15, "/gf/KZ"), 
                      bylayer = TRUE, suffix = names(rst_lai_crp_qc_md_adj_mrg), 
                      format = "GTiff", overwrite = TRUE)

fls_kz <- list.files(ch_dir_myd15, pattern = "^KZ_MRG_", 
                     full.names = TRUE, recursive = TRUE)
rst_kz <- stack(fls_kz)
mat_kz <- as.matrix(rst_kz)

# modis dates
ch_dt_fls_lai <- substr(basename(fls_kz), 31, 37)
dt_fls_lai <- as.Date(ch_dt_fls_lai, format = "%Y%j")


## lai extraction 

# 3-by-3 focal matrix per sls plot, or single pixel if `use_mat = FALSE`
ls_sls_lai <- lapply(1:nrow(df_sls_fls), function(i, use_mat = FALSE) {
  tmp_spt_plt <- subset(spt_plt, PlotID == df_sls_fls$plot[i])
  
  tmp_int_plt_px <- cellFromXY(rst_kz, tmp_spt_plt)
  
  if (use_mat) {
    tmp_int_plt_px_adj <- adjacent(rst_kz, tmp_int_plt_px, 
                                   sorted = TRUE, directions = 8, include = TRUE, 
                                   pairs = FALSE)
    tmp_mat_lai <- mat_kz[tmp_int_plt_px_adj, ]
    tmp_nms <- colnames(tmp_mat_lai)
  } else {
    tmp_num_lai <- mat_kz[tmp_int_plt_px, ]
    tmp_nms <- names(tmp_num_lai)
    tmp_mat_lai <- matrix(tmp_num_lai, 1, byrow = TRUE)
  }
  tmp_df_lai <- data.frame(tmp_mat_lai)
  names(tmp_df_lai) <- tmp_nms
  
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df_lai)
})
df_sls_lai <- do.call("rbind", ls_sls_lai)

# temporal range per plot
df_sls_tmp_rng <- slsTemporalRange(df_sls_fls)

ls_sls_lai_md <- lapply(1:nrow(df_sls_tmp_rng), function(i) {
  # subset lai data by current plot and season
  tmp_df_lai <- subset(df_sls_lai, plot == df_sls_tmp_rng$plot[i] &
                         season == df_sls_tmp_rng$season[i])
  
  # identify lai scene(s) corresponding to sls measurement
  tmp_df_sls_rng <- subset(df_sls_tmp_rng, plot == df_sls_tmp_rng$plot[i] &
                             season == df_sls_tmp_rng$season[i])
  tmp_int_id_lai <- slsGppIndex(df_sls = tmp_df_sls_rng, 
                                dt_fls_gpp = dt_fls_lai, offset = 2)
  
  tmp_df_sls_lai <- data.frame(tmp_df_sls_rng, tmp_df_lai[, tmp_int_id_lai])
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(tmp_df_sls_lai) > 5) {
    tmp_df <- tmp_df_sls_lai[5:ncol(tmp_df_sls_lai)]
    tmp_num_mu <- rowMeans(tmp_df, na.rm = TRUE)
    tmp_df_sls_lai <- data.frame(tmp_df_sls_lai[, 1:4], tmp_num_mu)
  }
  
  # mean lai
  data.frame(tmp_df_sls_lai[1, 1:4], 
             lai = mean(tmp_df_sls_lai[, 5], na.rm = TRUE))
})
df_sls_lai_md <- do.call("rbind", ls_sls_lai_md)
save("df_sls_lai_md", file = "data/modis_lai.rds")


### visualization

df_plt_lai <- readRDS("data/licor_lai.rds")
df_sls_lai <- readRDS("data/modis_lai.rds")
df_sls_lai_mu <- readRDS("data/modis_lai_mu.rds")

## create and rearrange habitat type factor levels
df_plt_lai$habitat <- substr(df_plt_lai$plot, 1, 3)

ch_lvl <- substr(slsPlots(style = "elevation"), 1, 3)
ch_lvl <- unique(ch_lvl)
df_plt_lai$habitat <- factor(df_plt_lai$habitat, levels = ch_lvl)

## lai means and standard errors per habitat type
df_plt_lai %>%
  group_by(habitat) %>% 
  filter(season == "r") %>%
  mutate(lai_mu = mean(LAI), lai_se = std.error(LAI)) %>%
  data.frame() -> df_hab_lai

## limits of error bars and y-axis
limits <- aes(ymax = lai_mu + lai_se, ymin = lai_mu - lai_se)
num_ylim <- c(0, max(df_hab_lai$lai_mu + df_hab_lai$lai_se, na.rm = TRUE) + .5)

## visualize
p_lai_rs <- ggplot(aes(x = habitat, y = lai_mu), data = df_hab_lai) + 
  geom_histogram(stat = "identity", position = "dodge", fill = "grey80", 
                 colour = "grey60", lwd = 1.2, alpha = .5) +
  geom_errorbar(limits, position = "dodge", linetype = "dashed", width = .2) + 
  geom_text(aes(x = habitat, y = LAI, label = plot), vjust = 1.6, 
            fontface = "bold", size = 6,
            ,subset = .(!is.na(lai_se) & plot %in% c("sav0", "mai0", "gra2", "cof3"))) + 
  geom_text(aes(x = habitat, y = LAI, label = plot), vjust = -1, 
            fontface = "bold", size = 6,
            ,subset = .(!is.na(lai_se) & plot %in% c("sav5", "mai4", "gra1", "cof2"))) +
  labs(x = "\nHabitat type", y = "LAI\n") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15)) + 
  coord_cartesian(ylim = num_ylim)

png(paste0(ch_dir_ppr, "fig/fig0x__lai.png"), width = 20, height = 15, 
    units = "cm", pointsize = 18, res = 300)
print(p_lai_rs)
dev.off()

# ## modis vs. in situ lai
# df_licor_modis_lai <- merge(df_sls_lai_md, df_plt_lai, by = c("plot", "season"))
# 
# xyplot(LAI ~ lai, data = df_licor_modis_lai, 
#        xlab = expression(LAI[MODIS]), ylab = expression(LAI[LICOR]), 
#        panel = function(x, y, ...) {
#   panel.xyplot(x, y, col = "grey50")
#   panel.text(x, y, labels = df_licor_modis_lai$plot, pos = 2, offset = .75)
# })

## deregister parallel backend
closeAllConnections()
