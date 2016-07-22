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

## quality-controlled lai
fls_lai <- list.files("data/MCD15A2H.006/qc2", 
                      pattern = "^MCD15A2H.*.tif$", full.names = TRUE)
dts_lai <- as.Date(extractDate(fls_lai, 11, 17)$inputLayerDates, "%Y%j")
fls_lai <- fls_lai[grep("2003", fls_lai)[1]:grep("2015361", fls_lai)]
rst_lai <- stack(fls_lai)
mat_lai <- as.matrix(rst_lai)

## quality-controlled ndvi
fls_ndvi <- foreach(product = c("MOD13A1.006", "MYD13A1.006"), 
                    .combine = "c") %do% {
  list.files(paste0("data/", product, "/qc2"), full.names = TRUE,
             pattern = paste0(strsplit(product, "\\.")[[1]][1], ".*.tif$"))
}

dts_ndvi <- as.Date(extractDate(fls_ndvi)$inputLayerDates, "%Y%j")
fls_ndvi <- fls_ndvi[order(dts_ndvi)]
fls_ndvi <- fls_ndvi[grep("2003", fls_ndvi)[1]:grep("2015361", fls_ndvi)]
rst_ndvi <- stack(fls_ndvi)
mat_ndvi <- as.matrix(rst_ndvi)

## quality-controlled wdrvi
dir_wdrvi <- "data/MCD13A1.006/qc2"
if (!dir.exists(dir_wdrvi)) dir.create(dir_wdrvi)

fls_wdrvi <- paste0(dir_wdrvi, "/", gsub("NDVI", "WDRVI", basename(fls_ndvi)))
rst_wdrvi <- wdrvi(ndvi = rst_ndvi, cores = 3L, filename = fls_wdrvi, 
                   format = "GTiff", overwrite = TRUE)
mat_wdrvi <- as.matrix(rst_wdrvi)


### linear regression -----

## correlation coefficient
fls_r <- paste0("data/results/", c("corLaiNdvi", "corLaiWdrvi"), ".tif")
lst_r <- foreach(i = list(mat_ndvi, mat_wdrvi), 
                 filename = fls_r, .packages = c("Orcs", "foreach")) %dopar% {
  val <- foreach(j = 1:nrow(mat_lai), .combine = "c") %do% {
    if (all(is.na(mat_lai[j, ])) | all(is.na(i[j, ])) |
        sum(!is.na(mat_lai[j, ])) < 23 | sum(!is.na(i[j, ])) < 23) {
      return(NA)
    }
    
    r <- cor(i[j, ], mat_lai[j, ], use = "complete.obs")
    if (is.na(r)) {
      return(NA)
    }
    
    mod <- lm(mat_lai[j, ] ~ i[j, ], na.action = na.omit)
    p <- pvalue(mod)
    
    if (p < 0.001) {
      return(r)
    } else {
      return(NA)
    }
  }
  
  rst <- setValues(rst_lai[[1]], val)
  writeRaster(rst, filename = filename, format = "GTiff", overwrite = TRUE)
}

## coefficient of determination
fls_rsq <- paste0("data/results/", c("rsqLaiNdvi", "rsqLaiWdrvi"), ".tif")
lst_rsq <- foreach(i = list(mat_ndvi, mat_wdrvi), 
                 filename = fls_rsq, .packages = c("Orcs", "foreach")) %dopar% {
  val <- foreach(j = 1:nrow(mat_lai), .combine = "c") %do% {
    if (all(is.na(mat_lai[j, ])) | all(is.na(i[j, ])) |
        sum(!is.na(mat_lai[j, ])) < 23 | sum(!is.na(i[j, ])) < 23) {
      return(NA)
    }
    
    mod <- lm(mat_lai[j, ] ~ i[j, ], na.action = na.omit)
    rsq <- summary(mod)$r.squared
    p <- pvalue(mod)
    
    if (!is.na(p) & p < 0.001) {
      return(rsq)
    } else {
      return(NA)
    }
  }
  
  rst <- setValues(rst_lai[[1]], val)
  writeRaster(rst, filename = filename, format = "GTiff", overwrite = TRUE)
}

## deregister parallel backend
stopCluster(cl)