### environmental stuff -----

## clear workspace
rm(list = ls(all = TRUE))

## packages
library(Orcs)
lib <- c("rgdal", "MODIS", "doParallel", "reset")
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
fls_ndvi <- list.files("data/MCD13A1.006/whittaker",
                      pattern = "^MCD13A1.*.tif$", full.names = TRUE)
dts_ndvi <- as.Date(extractDate(fls_ndvi)$inputLayerDates, "%Y%j")
rst_ndvi <- stack(fls_ndvi)
mat_ndvi <- as.matrix(rst_ndvi)

## whittaker-smoothed wdrvi
fls_wdrvi <- list.files("data/MCD13A1.006/whittaker/wdrvi", 
                       pattern = "^MCD13A1.*.tif$", full.names = TRUE)
dts_wdrvi <- as.Date(extractDate(fls_wdrvi)$inputLayerDates, "%Y%j")
rst_wdrvi <- stack(fls_wdrvi)
mat_wdrvi <- as.matrix(rst_wdrvi)


### linear regression -----

## correlation coefficient
dir_r <- "data/results"
# if (!dir.exists(dir_r)) dir.create(dir_r)
# 
# fls_r <- paste0(dir_r, c("/corLaiNdvi", "/corLaiWdrvi"), "Whittaker.tif")
# 
# lst_r <- foreach(i = list(mat_ndvi, mat_wdrvi),
#                  filename = fls_r, .packages = c("foreach", "Orcs")) %dopar% {
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

fls_r <- list.files(dir_r, pattern = "cor.*Whittaker.tif$", 
                    full.names = TRUE)
lst_r <- lapply(fls_r, raster)

## coefficient of determination
# fls_rsq <- paste0(dir_r, c("/rsqLaiNdvi", "/rsqLaiWdrvi"), "Whittaker.tif")
# 
# lst_rsq <- foreach(i = list(mat_ndvi, mat_wdrvi),
#                  filename = fls_rsq, .packages = c("foreach", "Orcs")) %dopar% {
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

fls_rsq <- list.files(dir_r, pattern = "rsq.*Whittaker.tif$", 
                      full.names = TRUE)
lst_rsq <- lapply(fls_r, raster)


### downscaling -----

## slope and intercept
# dat_cfs <- foreach(j = 1:nrow(mat_lai), .combine = "rbind") %do% {
#   x <- mat_wdrvi[j, ]; y <- mat_lai[j, ]; 
#   if (all(is.na(x)) | all(is.na(y))) 
#     return(NA)
# 
#   mod <- lm(y ~ x)
#   int <- coef(mod)[1]; slp <- coef(mod)[2]
#   
#   data.frame(cell = j, intercept = int, slope = slp)
# }
# 
# rst_int <- setValues(rst_lai[[1]], dat_cfs$intercept)
# rst_int <- writeRaster(rst_int, paste0(dir_r, "/intLaiWdrviWhittaker.tif"), 
#                        format = "GTiff", overwrite = TRUE)

rst_int <- raster(paste0(dir_r, "intLaiWdrviWhittaker.tif"))

# rst_slp <- setValues(rst_lai[[1]], dat_cfs$slope)
# rst_slp <- writeRaster(rst_slp, paste0(dir_r, "/slpLaiWdrviWhittaker.tif"), 
#                        format = "GTiff", overwrite = TRUE)

rst_slp <- raster(paste0(dir_r, "/slpLaiWdrviWhittaker.tif"))

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
mat_lai250 <- matrix(nrow = nrow(mat_wdrvi), ncol = ncol(mat_wdrvi))

for (i in 1:ncell(rst_int)) {
  
  ## status message
  if (i %% 50 == 0)
    cat("Now processing cell #", i, "...\n", sep = "")
  
  ## slope and intercept
  int <- rst_int[i]; slp <- rst_slp[i]
  if (is.na(int) | is.na(slp))
    next
  
  ## 250-m cells within current 500-m cell
  rst <- rst_int[[1]]
  rst[][-i] <- NA
  shp <- rasterToPolygons(rst)
  cls <- unlist(cellFromPolygon(rst_wdrvi, shp))
  
  ## apply regression coefficients
  val <- mat_wdrvi[cls, ]
  if (!is.matrix(val)) {
    mat_lai250[cls, ] <- val * slp + int
  } else {
    mat_lai250[cls, ] <- foreach(j = 1:nrow(val), .combine = "rbind") %do% {
      if (all(is.na(val[j, ])))
        return(rep(NA, ncol(mat_wdrvi)))
      
      val[j, ] * slp + int
    }
  }
}

rst_lai250 <- setValues(rst_wdrvi, mat_lai250)

## write to disk
fls_lai250 <- paste0(dir_wdrvi, "/", gsub("WDRVI", "LAI", names(rst_wdrvi)), ".tif")
lst_lai250 <- foreach(i = 1:nlayers(rst_lai250), .packages = "raster") %dopar%
  writeRaster(rst_lai250[[i]], fls_lai250[i], format = "GTiff", overwrite = TRUE)
rst_lai250 <- stack(lst_lai250)

## deregister parallel backend
stopCluster(cl)
