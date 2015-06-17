## required packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## parallelization
cl <- makeCluster(3)
registerDoParallel(cl)

## path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"

## path: output storage
ch_dir_ppr <- "/media/permanent/publications/paper/detsch_et_al__spotty_evapotranspiration/"

## data: sls measurements
df_sls_fls <- slsAvlFls()

## data: coordinates
spt_plt <- readOGR(ch_dir_crd, ch_fls_crd, p4s = "+init=epsg:21037")
spt_plt <- subset(spt_plt, PoleType == "AMP")
spt_plt <- subset(spt_plt, PlotID %in% slsPlots())

## reference extent
rst <- stack("../../kilimanjaro/kili_bing.tif")

## hdf files
ch_hdf <- list.files("/media/fdetsch/FREECOM_HDD/MODIS_ARC/MODIS", 
                     full.names = TRUE, pattern = ".hdf$")

## parameter file
ch_prm <- readLines("data/mod16.prm")

## target directory
ch_dir_out <- "../../phd/scintillometer/data/mod16/"

## loop over files
int_id_input <- grep("INPUT_FILENAME", ch_prm)
int_id_output <- grep("OUTPUT_FILENAME", ch_prm)

for (i in ch_hdf) {
  
  ch_prm[int_id_input] <- paste("INPUT_FILENAME", i, sep = " = ")
  
  tmp_ch_hdf <- basename(i)
  tmp_ch_hdf <- paste0(ch_dir_out, tmp_ch_hdf)
  tmp_ch_tif <- gsub(".hdf", ".tif", tmp_ch_hdf)
  
  ch_prm[int_id_output] <- paste("OUTPUT_FILENAME", tmp_ch_tif, sep = " = ")
  
  writeLines(ch_prm, "data/mod16.prm")
  
  system("resample -p data/mod16.prm")
}

## import extracted files
ch_fls_pet <- list.files(ch_dir_out, pattern = "_1km.tif$", 
                         full.names = TRUE)
rst_pet <- stack(ch_fls_pet)

## reprojection
rst_pet_prj <- foreach(i = 1:nlayers(rst_pet), .packages = lib, 
                  .combine = "stack") %dopar% {
  projectRaster(rst_pet[[i]], crs = "+init=epsg:21037", 
                filename = paste0(ch_dir_out, "/prj/PRJ_", names(rst_pet)[i]), 
                format = "GTiff", overwrite = TRUE)
}

## clipping
rst_pet_crp <- foreach(i = 1:nlayers(rst_pet_prj), .packages = lib, 
                       .combine = "stack") %dopar% {
  crop(rst_pet_prj[[i]], rst, 
       filename = paste0(ch_dir_out, "/crp/CRP_", names(rst_pet_prj)[i]), 
       format = "GTiff", overwrite = TRUE)
}

## quality control
rst_pet <- rst_pet_crp[[grep("PET_1km", ch_fls_pet)]]
rst_qc <- rst_pet_crp[[grep("ET_QC_1km", ch_fls_pet)]]

rst_pet_qc <- foreach(i = 1:nlayers(rst_pet), j = names(rst_pet),
        .packages = lib, .combine = "stack") %dopar% {
          overlay(rst_pet[[i]], rst_qc[[i]], fun = function(x, y) {
            num_x <- x[]
            num_y <- y[]
            num_y_qc <- sapply(num_y, function(z) qcMyd17(z))
            log_y_qc <- is.na(num_y_qc)
            num_x[log_y_qc] <- NA
            return(num_x)
          }, filename = paste0(ch_dir_out, "/qc/QC_", j), format = "GTiff", 
          overwrite = TRUE)
        }

## outlier check
rst_pet_sd <- calc(rst_pet_qc, fun = function(x) {
  log_na <- is.na(x)
  
  if (sum(log_na) / length(x) <= .8) {
    id <- tsOutliers(x, lower_quantile = .3, upper_quantile = .9, 
                     index = TRUE)
    x[id] <- NA
  }
  
  return(x)
})
rst_pet_sd <- writeRaster(rst_pet_sd, filename = paste0(ch_dir_out, "/sd/SD"), 
                          format = "GTiff", bylayer = TRUE, 
                          suffix = names(rst_pet_qc), overwrite = TRUE)

## reclassify
rst_pet_rcl <- foreach(i = 1:nlayers(rst_pet_sd), .packages = lib, 
                      .combine = "stack") %dopar% {
  tmp_rst <- rst_pet_sd[[i]]
  int_val <- getValues(tmp_rst)
  int_id_msk <- which(int_val >= 2000)
  tmp_rst[int_id_msk] <- 0
  
  tmp_rst <- tmp_rst * .1
  
  writeRaster(tmp_rst, format = "GTiff", overwrite = TRUE,
              filename = paste0(ch_dir_out, "/rcl/RCL_", names(tmp_rst)))
}
mat_pet_rcl <- raster::as.matrix(rst_pet_rcl)

## gap-filling (kza(..., k = 1) is equal to moving average)
rst_kz <- calc(rst_pet_rcl, function(x) {
  int_na <- which(is.na(x))
  
  if (length(int_na) > 0) {
    tmp_num_kz <- kza(x, m = 3, k = 3, impute_tails = TRUE)$kz
    x[int_na] <- tmp_num_kz[int_na]
  }
  
  return(x)
})
rst_kz <- writeRaster(rst_kz, filename = paste0(ch_dir_out, "/gf/KZ"), 
                      format = "GTiff", bylayer = TRUE, 
                      suffix = names(rst_pet_rcl), overwrite = TRUE)


fls_kz <- list.files(ch_dir_out, pattern = "^KZ_", 
                     full.names = TRUE, recursive = TRUE)
rst_kz <- stack(fls_kz)
mat_kz <- raster::as.matrix(rst_kz)

## et_pot extraction 

# modis dates
ch_dt_fls_pet <- substr(basename(fls_kz), 31, 37)
dt_fls_pet <- as.Date(ch_dt_fls_pet, format = "%Y%j")

# 3-by-3 focal matrix per sls plot, or single pixel if `use_mat = FALSE`
ls_sls_pet <- lapply(1:nrow(df_sls_fls), function(i, use_mat = FALSE) {
  tmp_spt_plt <- subset(spt_plt, PlotID == df_sls_fls$plot[i])
  
  tmp_int_plt_px <- cellFromXY(rst_pet_rcl, tmp_spt_plt)
  
  if (use_mat) {
    tmp_int_plt_px_adj <- adjacent(rst_pet_rcl, tmp_int_plt_px, 
                                   sorted = TRUE, directions = 8, include = TRUE, 
                                   pairs = FALSE)
    tmp_mat_pet <- mat_kz[tmp_int_plt_px_adj, ]
    tmp_nms <- colnames(tmp_mat_pet)
  } else {
    tmp_num_pet <- mat_kz[tmp_int_plt_px, ]
    tmp_nms <- names(tmp_num_pet)
    tmp_mat_pet <- matrix(tmp_num_pet, 1, byrow = TRUE)
  }
  tmp_df_pet <- data.frame(tmp_mat_pet)
  names(tmp_df_pet) <- tmp_nms
  
  data.frame(plot = df_sls_fls$plot[i], season = df_sls_fls$season[i], tmp_df_pet)
})
df_sls_pet <- do.call("rbind", ls_sls_pet)

# temporal range per plot
df_sls_tmp_rng <- slsTemporalRange(df_sls_fls)

ls_sls_pet_md <- lapply(1:nrow(df_sls_tmp_rng), function(i) {
  # subset pet data by current plot and season
  tmp_df_pet <- subset(df_sls_pet, plot == df_sls_tmp_rng$plot[i] &
                         season == df_sls_tmp_rng$season[i])
  
  # identify pet scene(s) corresponding to sls measurement
  tmp_df_sls_rng <- subset(df_sls_tmp_rng, plot == df_sls_tmp_rng$plot[i] &
                             season == df_sls_tmp_rng$season[i])
  tmp_int_id_pet <- slsGppIndex(df_sls = tmp_df_sls_rng, 
                                dt_fls_gpp = dt_fls_pet, offset = 2)
  
  tmp_df_sls_pet <- data.frame(tmp_df_sls_rng, tmp_df_pet[, tmp_int_id_pet])
  
  # calculate mean if sls measurement includes two modis scenes
  if (ncol(tmp_df_sls_pet) > 5) {
    tmp_df <- tmp_df_sls_pet[5:ncol(tmp_df_sls_pet)]
    tmp_num_mu <- rowMeans(tmp_df, na.rm = TRUE)
    tmp_df_sls_pet <- data.frame(tmp_df_sls_pet[, 1:4], tmp_num_mu)
  }
  
  # median pet
  data.frame(tmp_df_sls_pet[1, 1:4], 
             pet = median(tmp_df_sls_pet[, 5], na.rm = TRUE))
})
df_sls_pet_md <- do.call("rbind", ls_sls_pet_md)
save("df_sls_pet_md", file = "data/pet.RData")
