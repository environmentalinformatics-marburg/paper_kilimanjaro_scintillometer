### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
library(Orcs)
lib <- c("rgdal", "MODIS", "doParallel")
loadPkgs(lib)

## set working directory
dir_old <- getwd()
setwdOS(path_lin = "/media/fdetsch/Permanent", path_win = "E:/", 
        path_ext = "phd/scintillometer")

## parallelization
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


### data import -----

## whittaker-smoothed lai
fls_lai <- list.files("data/MCD15A2H.006/whittaker", 
                      pattern = "^MCD15A2H.*.tif$", full.names = TRUE)
dts_lai <- as.Date(extractDate(fls_lai, 11, 17)$inputLayerDates, "%Y%j")
rst_lai <- stack(fls_lai)
mat_lai <- as.matrix(rst_lai)

## whittaker-smoothed ndvi
# fls_ndvi <- list.files("data/MCD13A1.006/whittaker", 
#                       pattern = "^MCD13A1.*.tif$", full.names = TRUE)
# dts_ndvi <- as.Date(extractDate(fls_ndvi)$inputLayerDates, "%Y%j")
# rst_ndvi <- stack(fls_ndvi)
# mat_ndvi <- as.matrix(rst_ndvi)

## whittaker-smoothed wdrvi
fls_wdrvi <- list.files("data/MCD13A1.006/whittaker/wdrvi", 
                       pattern = "^MCD13A1.*.tif$", full.names = TRUE)
dts_wdrvi <- as.Date(extractDate(fls_wdrvi)$inputLayerDates, "%Y%j")
rst_wdrvi <- stack(fls_wdrvi)
mat_wdrvi <- as.matrix(rst_wdrvi)


### linear regression -----

## correlation coefficient
# fls_r <- paste0("data/results/", c("corLaiNdvi", "corLaiWdrvi"), "Whittaker.tif")
# lst_r <- foreach(i = list(mat_ndvi, mat_wdrvi), 
#                  filename = fls_r, .packages = "Orcs") %dopar% {
#   val <- foreach(j = 1:nrow(mat_lai), .combine = "c") %do% {
#     r <- cor(i[j, ], mat_lai[j, ])
#     if (is.na(r)) {
#       return(NA)
#     }
#     
#     mod <- lm(mat_lai[j, ] ~ i[j, ])
#     p <- pvalue(mod)
#     
#     if (p < 0.001) {
#       return(r)
#     } else {
#       return(NA)
#     }
#   }
#   
#   rst <- setValues(rst_lai[[1]], val)
#   writeRaster(rst, filename = filename, format = "GTiff", overwrite = TRUE)
# }

fls_r <- list.files("data/results", pattern = "cor.*Whittaker.tif$", 
                    full.names = TRUE)
lst_r <- lapply(fls_r, raster)

## coefficient of determination
# fls_rsq <- paste0("data/results/", c("rsqLaiNdvi", "rsqLaiWdrvi"), "Whittaker.tif")
# lst_rsq <- foreach(i = list(mat_ndvi, mat_wdrvi), 
#                  filename = fls_rsq, .packages = "Orcs") %dopar% {
#   val <- foreach(j = 1:nrow(mat_lai), .combine = "c") %do% {
#     if (all(is.na(mat_lai[j, ])) | all(is.na(i[j, ]))) {
#       return(NA)
#     }
#     
#     mod <- lm(mat_lai[j, ] ~ i[j, ])
#     rsq <- summary(mod)$r.squared
#     p <- pvalue(mod)
#     
#     if (p < 0.001) {
#       return(rsq)
#     } else {
#       return(NA)
#     }
#   }
#   
#   rst <- setValues(rst_lai[[1]], val)
#   writeRaster(rst, filename = filename, format = "GTiff", overwrite = TRUE)
# }

fls_rsq <- list.files("data/results", pattern = "rsq.*Whittaker.tif$", 
                      full.names = TRUE)
lst_rsq <- lapply(fls_r, raster)


### downscaling -----

## slope and intercept
dat_cfs <- foreach(j = 1:nrow(mat_lai), .combine = "rbind") %do% {
  x <- mat_wdrvi[j, ]; y <- mat_lai[j, ]; 
  if (all(is.na(x)) | all(is.na(y))) 
    return(NA)

  mod <- lm(y ~ x)
  int <- coef(mod)[1]; slp <- coef(mod)[2]
  
  data.frame(cell = j, intercept = int, slope = slp)
}

rst_int <- setValues(rst_lai[[1]], dat_cfs$intercept)
rst_int <- writeRaster(rst_int, "data/results/intLaiWdrviWhittaker.tif", 
                       format = "GTiff", overwrite = TRUE)

rst_slp <- setValues(rst_lai[[1]], dat_cfs$slope)
rst_slp <- writeRaster(rst_slp, "data/results/slpLaiWdrviWhittaker.tif", 
                       format = "GTiff", overwrite = TRUE)

## 250-m wdrvi
fls_ndvi <- list.files("data/MCD13Q1.006/whittaker", pattern = "NDVI.tif$", 
                       full.names = TRUE)
rst_ndvi <- stack(fls_ndvi)

dir_wdrvi <- unique(dirname(fls_ndvi))
fls_wdrvi <- gsub("NDVI", "WDRVI", fls_ndvi)
rst_wdrvi <- wdrvi(ndvi = rst_ndvi, cores = 3L, filename = fls_wdrvi, 
                   format = "GTiff", overwrite = TRUE)
mat_wdrvi <- as.matrix(rst_wdrvi)

## cell numbers
mat_lai250 <- foreach(i = 1:ncell(rst_int), .combine = "rbind", 
                      .packages = c("raster", "foreach")) %do% {
  
  int <- rst_int[i]; slp <- rst_slp[i]
  if (is.na(int) | is.na(slp))
    return(rep(NA, ncol(mat_wdrvi)))
  
  rst <- rst_int[[1]]
  rst[][-i] <- NA
  shp <- rasterToPolygons(rst)
  cls <- unlist(cellFromPolygon(rst_wdrvi, shp))

  val <- mat_wdrvi[cls, ]
  if (!is.matrix(val)) {
    val * slp + int
  } else {
    foreach(j = 1:nrow(val), .combine = "rbind") %do% {
      if (all(is.na(val[j, ])))
        return(rep(NA, ncol(mat_wdrvi)))
      
      val[j, ] * slp + int
    }
  }
}


## deregister parallel backend
stopCluster(cl)