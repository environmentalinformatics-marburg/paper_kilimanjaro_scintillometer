## environmental stuff

# packages and functions
source("R/slsPkgs.R")
library(GSODTools)

source("R/slsFcts.R")

# parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

# path: modis, raw and extracted
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

# path: modis, processed
ch_dir_myd17 <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/myd17a2/"

# path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

# path: output storage
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

# modis options
MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)


## data

# clipping extent
rst_tmp <- kiliAerial(minNumTiles = 9L, rasterize = TRUE)

# data download
ls_md17_gpp <- lapply(c("MOD", "MYD"), function(sensor, exe_crop = TRUE, 
                                                exe_qc = TRUE) {
  
  # modis data download
  product <- paste0(sensor, "17A2")
  #   runGdal(product, collection = "005", begin = "2013001", tileH = 21, tileV = 9, 
  #           SDSstring = "101", job = "GPPkili", outProj = "+init=epsg:21037")
  
  # modis dates
  ch_fls_gpp <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                           pattern = paste0(product, ".*Gpp_1km.tif$"))
  ls_fls_gpp <- extractDate(ch_fls_gpp, asDate = TRUE)
  dt_fls_gpp <- ls_fls_gpp$inputLayerDates

  # data import and cropping
  if (exe_crop) {
    ls_myd17_crp <- foreach(sds = c("Gpp", "QC"), scale_factor = c(0.0001, 1), 
                            .packages = c("raster", "rgdal", "foreach")) %do% {
      tmp_ch_fls <- list.files(ch_dir_prc, full.names = TRUE, recursive = TRUE, 
                               pattern = paste0(product, ".*", sds, "_1km.tif$"))
      
      tmp_rst_crp <- foreach(i = tmp_ch_fls, .combine = "stack") %do% {
        tmp_rst <- raster(i)
        tmp_rst_scl <- tmp_rst * scale_factor
        
        crop(tmp_rst_scl, rst_tmp, format = "GTiff", overwrite = TRUE,
             filename = paste0(ch_dir_myd17, "/crp/CRP_", basename(i)))
      }
      
      return(tmp_rst_crp)
    }
    
    rst_gpp <- ls_myd17_crp[[1]]
    rst_qc <- ls_myd17_crp[[2]]
    
  } else {
    fls_gpp <- list.files(paste0(ch_dir_myd17, "/crp"), 
                          pattern = paste("^CRP", sensor, "Gpp", sep = ".*"), 
                          full.names = TRUE)
    rst_gpp <- stack(fls_gpp)
    
    fls_qc <- list.files(paste0(ch_dir_myd17, "/crp"), pattern = "^CRP.*Psn_QC", 
                         full.names = TRUE)
    rst_qc <- stack(fls_qc)
  }

  # qc
  if (exe_qc) {
    rst_myd17_crp_qc <- foreach(i = 1:nlayers(rst_gpp), j = names(rst_gpp),
                                .packages = c("rgdal", "raster"), 
                                .combine = "stack") %do% {
      overlay(rst_gpp[[i]], rst_qc[[i]], fun = function(x, y) {
        num_x <- x[]
        num_y <- y[]
        num_y_qc <- sapply(num_y, function(i) qcMyd17(i, cs_exclude = c("10", "01")))
        log_y_qc <- is.na(num_y_qc)
        num_x[log_y_qc] <- NA
        return(num_x)
      }, filename = paste0(ch_dir_myd17, "/qc/QC_", j), format = "GTiff", 
      overwrite = TRUE)
    }
  } else {
    fls_myd17_crp_qc <- list.files(ch_dir_myd17, pattern = "^QC", 
                                   full.names = TRUE, recursive = TRUE)
    rst_myd17_crp_qc <- stack(fls_myd17_crp_qc)
  }

  #   # tsOutliers
  #   if (exe_tso) {
  #     rst_myd17_crp_qc_tso <- calc(rst_myd17_crp_qc, fun = function(x) {
  #       tmp_num <- as.numeric(x)
  #       
  #       if (!all(is.na(tmp_num))) {
  #         tmp_int_id <- tsOutliers(tmp_num, lower_quantile = .5, upper_quantile = .8, 
  #                                  index = TRUE)
  #         if (length(tmp_int_id) > 0)
  #           tmp_num[tmp_int_id] <- NA
  #       }
  #       
  #       return(tmp_num)
  #     }, filename = paste0(ch_dir_myd17, "/tso/TSO"), bylayer = TRUE, 
  #     suffix = names(rst_myd17_crp_qc), format = "GTiff", overwrite = TRUE)
  #   } else {
  #     fls_myd17_crp_qc_tso <- list.files(ch_dir_myd17, pattern = "^TSO", 
  #                                        full.names = TRUE, recursive = TRUE)
  #     rst_myd17_crp_qc_tso <- stack(fls_myd17_crp_qc_tso)
  #   }
  #     
  #   return(rst_myd17_crp_qc_tso)
  # })
 
  return(rst_myd17_crp_qc)
}

# overlay terra and aqua modis
ls_md17_qc <- lapply(c("MOD", "MYD"), function(i) {
  tmp_ch_fls <- list.files(ch_dir_myd17, pattern = paste0("^QC.*", i), 
                           full.names = TRUE, recursive = TRUE)
  stack(tmp_ch_fls)
})

rst_md17_mrg <- foreach(i = 1:nlayers(ls_md17_qc[[2]]), .combine = "stack",
                        .packages = c("raster", "rgdal")) %dopar% {
  overlay(ls_md17_qc[[1]][[i]], ls_md17_qc[[2]][[i]], 
          fun = function(x, y) {
            sapply(1:length(x), function(i) {
              nax <- is.na(x[i])
              nay <- is.na(y[i])
              
              if (nax & nay) {
                return(NA)
              } else if (!nax & nay) {
                return(x[i])
              } else if (nax & !nay) {
                return(y[i])
              } else {
                return(max(x[i], y[i]))
              }
            })
          }, filename = paste0(ch_dir_myd17, "/mrg/MRG_", names(ls_md17_qc[[1]])[i]), 
          format = "GTiff", overwrite = TRUE)
}


## gap-filling

# kolmogorov-zurbenko adaptive (if k = 1 -> identical to moving average)
rst_md17_mrg_kz <- calc(rst_md17_mrg, fun = function(i) {
  tmp_num_gpp <- i
  tmp_log_isna <- is.na(tmp_num_gpp)
  
  tmp_num_mav <- kza(tmp_num_gpp, m = 3, k = 3, impute_tails = TRUE)
  
  tmp_num_gpp[tmp_log_isna] <- tmp_num_mav$kz[tmp_log_isna]
  return(tmp_num_gpp)
}, filename = paste0(ch_dir_myd17, "/gf/KZ"), bylayer = TRUE, 
suffix = names(rst_md17_mrg), format = "GTiff", overwrite = TRUE)

fls_md17_mrg_kz <- list.files(ch_dir_myd17, pattern = "^KZ_MRG", 
                                      full.names = TRUE, recursive = TRUE)
rst_md17_mrg_kz <- stack(fls_md17_mrg_kz)
mat_md17_mrg_kz <- as.matrix(rst_md17_mrg_kz)

# plot extraction (3-by-3 matrix)
spt_crd <- readOGR(dsn = ch_dir_crd, layer = ch_fls_crd, 
                   p4s = "+init=epsg:21037")
spt_crd_amp <- subset(spt_crd, PoleType == "AMP")

## data: sls measurements
df_sls_fls <- slsAvlFls()

ls_sls_gpp <- lapply(1:nrow(df_sls_fls), function(i, use_mat = FALSE) {
  tmp_spt_crd_amp <- subset(spt_crd_amp, PlotID == df_sls_fls$plot[i])
  tmp_int_crd_px <- cellFromXY(rst_md17_mrg_kz, tmp_spt_crd_amp)
  
  if (use_mat) {
    tmp_int_crd_px_adj <- adjacent(rst_md17_mrg_kz, tmp_int_crd_px,
                                   sorted = TRUE, directions = 8, include = TRUE,
                                   pairs = FALSE)
    tmp_mat_gpp <- mat_md17_mrg_kz[tmp_int_crd_px_adj, ]
    tmp_nms <- colnames(tmp_mat_gpp)
  } else {
    tmp_num_gpp <- mat_md17_mrg_kz[tmp_int_crd_px, ]
    tmp_nms <- names(tmp_num_gpp)
    tmp_mat_gpp <- matrix(tmp_num_gpp, 1, byrow = TRUE)
  }
    
  tmp_df_gpp <- data.frame(tmp_mat_gpp)
  names(tmp_df_gpp) <- tmp_nms
  
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df_gpp)
})
df_sls_gpp <- do.call("rbind", ls_sls_gpp)

# extraction of temporal range per plot
df_sls_tmp_rng <- slsTemporalRange(df_sls_fls)

ls_sls_gpp_md <- lapply(1:nrow(df_sls_tmp_rng), function(i) {
  # subset gpp data by current plot and season
  tmp_df_gpp <- subset(df_sls_gpp, plot == df_sls_tmp_rng$plot[i] &
                         season == df_sls_tmp_rng$season[i])
  
  # identify gpp scene(s) corresponding to sls measurement
  tmp_df_sls_rng <- subset(df_sls_tmp_rng, plot == df_sls_tmp_rng$plot[i] &
                             season == df_sls_tmp_rng$season[i])
  tmp_int_id_gpp <- slsGppIndex(df_sls = tmp_df_sls_rng, 
                                dt_fls_gpp = dt_fls_gpp, offset = 2)
  
  num_val <- tmp_df_gpp[, tmp_int_id_gpp]
  
  if (is.numeric(num_val)) {
    mat_val <- matrix(num_val, ncol = length(num_val))
  } else {
    mat_val <- t(as.matrix(num_val, ncol = length(num_val)))
  }
    tmp_df_sls_gpp <- data.frame(tmp_df_sls_rng, mat_val)
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(tmp_df_sls_gpp) > 5) {
    tmp_df <- tmp_df_sls_gpp[5:ncol(tmp_df_sls_gpp)]
    tmp_num_mu <- rowMeans(tmp_df, na.rm = TRUE)
    tmp_df_sls_gpp <- data.frame(tmp_df_sls_gpp[, 1:4], tmp_num_mu)
  }
  
  # calculate focal mean if cell of interest is missing, else return value
  return(data.frame(tmp_df_sls_gpp[1, 1:4], gpp = mean(tmp_df_sls_gpp[, 5])))
})
df_sls_gpp_md <- do.call("rbind", ls_sls_gpp_md)
save("df_sls_gpp_md", file = "data/modis_gpp.rds")

### visualization

## create and rearrange habitat type factor levels
df_sls_gpp_md$habitat <- substr(df_sls_gpp_md$plot, 1, 3)

ch_lvl <- substr(slsPlots(style = "elevation"), 1, 3)
ch_lvl <- unique(ch_lvl)
df_sls_gpp_md$habitat <- factor(df_sls_gpp_md$habitat, levels = ch_lvl)

## lai means and standard errors per habitat type
df_sls_gpp_md %>%
  group_by(habitat) %>% 
  filter(season == "r") %>%
  mutate(gpp_mu = mean(gpp), gpp_se = std.error(gpp)) %>%
  data.frame() -> df_hab_gpp

## limits of error bars and y-axis
limits <- aes(ymax = gpp_mu + gpp_se, ymin = gpp_mu - gpp_se)
num_ylim <- c(0, max(df_hab_gpp$gpp_mu + df_hab_gpp$gpp_se, na.rm = TRUE) + .01)

## visualize
p_gpp_rs <- ggplot(aes(x = habitat, y = gpp_mu), data = df_hab_gpp) + 
  geom_histogram(stat = "identity", position = "dodge", fill = "grey80", 
                 colour = "grey60", lwd = 1.2, alpha = .5) +
  geom_errorbar(limits, position = "dodge", linetype = "dashed", width = .2) + 
  geom_text(aes(x = habitat, y = gpp, label = plot), vjust = 1.6, 
            fontface = "bold", size = 6,
            ,subset = .(!is.na(gpp_se) & plot %in% c("sav0", "mai0", "gra2", "cof2"))) + 
  geom_text(aes(x = habitat, y = gpp, label = plot), vjust = -1, 
            fontface = "bold", size = 6,
            ,subset = .(!is.na(gpp_se) & plot %in% c("sav5", "mai4", "gra1", "cof3"))) +
  labs(x = "\nHabitat type", y = expression(atop("GPP (kgC/" ~ m^{2} * ")", ""))) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 15)) + 
  coord_cartesian(ylim = num_ylim)

png(paste0(ch_dir_ppr, "fig/fig0x__gpp.png"), width = 20, height = 15, 
    units = "cm", pointsize = 18, res = 300)
print(p_gpp_rs)
dev.off()

# deregister parallel backend
closeAllConnections()
