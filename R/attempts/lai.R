### environment ----------------------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## source functions
source("R/uniformExtent.R")

## set working directory
library(Orcs)
setwdOS(path_lin = "/media/fdetsch/dev", path_win = "D:/")

## load packages
lib <- c("Rsenal", "MODIS", "doParallel", "kza")
Orcs::loadPkgs(lib)

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

## modis options
MODIS:::MODISoptions(localArcPath = paste0(getwd(), "/data/MODIS_ARC/"), 
             outDirPath = paste0(getwd(), "/data/MODIS_ARC/PROCESSED/"), 
             outProj = "+init=epsg:21037")


### data download --------------------------------------------------------------

# ## combinded terra/aqua product
# MODIS:::runGdal(product = "MCD15A2H", 
#         collection = MODIS:::getCollection("MCD15A2H", forceCheck = TRUE),
#         tileH = 21, tileV = 9, SDSstring = "011100", job = "MCD15A2H.006")


### crop layers ----------------------------------------------------------------

## reference extent
ext_crp <- uniformExtent()

dir_prd <- "data/MCD15A2H.006"
if (!dir.exists(dir_prd)) dir.create(dir_prd)

## crop images
rst_crp <- foreach(i = c("Lai_500m", "Lai_QC", "Extra_QC"), 
                   .packages = lib) %dopar% {                                      
                     
  # list and import available files
  fls <- list.files(paste0(getOption("MODIS_outDirPath"), "/MCD15A2H.006"),
                    pattern = paste0(i, ".tif$"), full.names = TRUE)
  rst <- raster::stack(fls)
                     
  # crop
  dir_out <- paste0(dir_prd, "/crp")
  if (!dir.exists(dir_out)) dir.create(dir_out)
  
  fls_out <- paste0(dir_out, "/", basename(fls))
  
  lst_out <- lapply(1:(raster::nlayers(rst)), function(j) {
    if (file.exists(fls_out[j])) {
      raster::raster(fls_out[j])
    } else {
      rst_out <- raster::crop(rst[[j]], ext_crp, snap = "out")
      
      # apply scale factor
      if (i == "Lai_500m")
        rst_out <- rst_out * 0.1
      
      # save and return cropped layers
      raster::writeRaster(rst_out, filename = fls_out[j],
                          format = "GTiff", overwrite = TRUE)
    }
  })
  
  raster::stack(lst_out)
}


### quality control, step #1: -----
### discard clouds, snow/ice and filled pixels using 'pixel_reliability'

dir_qc1 <- paste0(dir_prd, "/qc1")
if (!dir.exists(dir_qc1)) dir.create(dir_qc1)

fls_qc1 <- paste0(dir_qc1, "/", names(rst_crp[[1]]), ".tif")

## perform quality check #1
lst_qc1 <- foreach(i = 1:nlayers(rst_crp[[1]]), .packages = lib) %dopar% {
  if (file.exists(fls_qc1[i])) {
    raster(fls_qc1[i])
  } else {
    overlay(rst_crp[[1]][[i]], rst_crp[[2]][[i]], fun = function(x, y) {
      id <- sapply(y[], function(k) {
        bin <- number2binary(k, 8, TRUE)
        modland <- substr(bin, 8, 8)
        
        if (modland == "0") {
          return(TRUE)
        } else {
          cloud_state <- substr(bin, 4, 5) %in% c("00", "11")
          confidence <- substr(bin, 1, 3) %in% c("000", "001")
          
          all(cloud_state, confidence)
        }
      })
      
      x[!id] <- NA
      return(x)
    }, filename = fls_qc1[i], overwrite = TRUE, format = "GTiff")
  }
}

rst_qc1 <- stack(lst_qc1)


### quality control, step #2: -----
### discard cloudy pixels based on 'state_250m' flags

dir_qc2 <- paste0(dir_prd, "/qc2")
if (!dir.exists(dir_qc2)) dir.create(dir_qc2)

fls_qc2 <- paste0(dir_qc2, "/", names(rst_qc1), ".tif")

## perform quality check #2
lst_qc2 <- foreach(i = 1:nlayers(rst_qc1), .packages = lib) %dopar% {
  if (file.exists(fls_qc2[i])) {
    raster(fls_qc2[i])
  } else {
    overlay(rst_qc1[[i]], rst_crp[[3]][[i]], fun = function(x, y) {
      id <- sapply(y[], function(k) {
        bin <- number2binary(k, 8, TRUE)
        snow <- substr(bin, 6, 6) == "0"
        cirrus <- substr(bin, 4, 4) == "0"
        intern_cloud <- substr(bin, 3, 3) == "0"
        cloud_shadow <- substr(bin, 2, 2) == "0"
        
        all(snow, cirrus, cloud_shadow)
      })
      
      x[!id] <- NA
      return(x)
    }, filename = fls_qc2[i], overwrite = TRUE, format = "GTiff")
  }
}

rst_qc2 <- raster::stack(lst_qc2)


### quality control, step #3: --------------------------------------------------
### discard pixels adjacent to clouds

# ## target folder and foles
# dir_adj <- paste0(dir_prd, "/qc3")
# if (!dir.exists(dir_adj)) dir.create(dir_adj)
# 
# fls_qc3 <- paste0(dir_adj, "/", names(rst_qc2), ".tif")
# 
# ## matrix of weights
# w <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3)
# 
# lst_qc3 <- foreach(i = 1:nlayers(rst_qc2), .packages = "raster") %dopar% {
#   if (file.exists(fls_qc3[i])) {
#     raster(fls_qc3[i])
#   } else {
#     cells <- which(is.na(rst_qc2[[i]][]))
#     
#     adj <- raster::adjacent(rst_qc2[[i]], cells, pairs = FALSE)
#     val <- getValues(rst_qc2[[i]])
#     val[adj] <- NA
#     rst <- setValues(rst_qc2[[i]], val)
#     
#     writeRaster(rst, fls_qc3[i], format = "GTiff", overwrite = TRUE)
#   }
# }
# 
# rst_qc3 <- stack(lst_qc3)


### whittaker smoother -----

## target folders and files
dir_wht <- paste0(dir_prd, "/whittaker")
if (!dir.exists(dir_wht)) dir.create(dir_wht)

## select temporal range
st <- grep("2003", names(rst_qc2))[1]
nd <- grep("2015", names(rst_qc2)); nd <- nd[length(nd)]
rst_qc2 <- rst_qc2[[st:nd]]

# detach("package:MODIS", unload = TRUE)
# install.packages("/media/fdetsch/dev/data/MODIS_0.10-18.tar.gz",
#                  repos = NULL, type = "source")
# library(MODIS)

## apply whittaker smoother
lst_wht <- whittaker.raster(rst_qc2, outDirPath = dir_wht,
                            timeInfo = orgTime(rst_qc2, pos1 = 11, pos2 = 17),
                            overwrite = TRUE, format = "GTiff")

## write to disc
rst_wht <- stack(lst_wht)
nms_qc2 <- names(rst_qc2)
fls_wht <- paste0(dir_wht, "/", nms_qc2, ".tif")

lst_wht <- foreach(i = 1:nlayers(rst_wht), .packages = "raster") %dopar% {
  rst <- rst_wht[[i]]
  rst[rst[] < 0] <- NA
  rst[rst[] > 10] <- NA
  
  writeRaster(rst, filename = fls_wht[i], format = "GTiff", overwrite = TRUE)
}

rst_wht <- stack(lst_wht)

## remove deprecated whittaker-related files
fls_old <- list.files(dir_wht, pattern = "NDVI_YearlyLambda", 
                      full.names = TRUE)
file.remove(fls_old)

## close parallel backend
stopCluster(cl)
