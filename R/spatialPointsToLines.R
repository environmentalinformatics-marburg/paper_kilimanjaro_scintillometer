spatialPointsToLines <- function(sp, ...) {
  
  stopifnot(require(rgdal))
  stopifnot(require(raster))
  
  mat <- coordinates(sp)
  
  p4 <- proj4string(sp)
  ln <- Line(mat)
  lns <- Lines(list(ln), ID = "path_length")
  
  sl <- SpatialLines(list(lns), proj4string = CRS(p4))
  return(sl)
}