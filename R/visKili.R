visKili <- function() {
  
  library(rworldmap)
  library(Rsenal)
  
  data("countriesCoarse")
  
  ## africa
  spy_africa <- subset(countriesCoarse, REGION == "Africa")
  
  ## tanzania
  spy_tanzania <- subset(countriesCoarse, POSTAL == "TZ")
  
  ## kilimanjaro
  rst_kili <- kiliAerial(projection = proj4string(spy_africa), rasterize = TRUE)
  
  spt_kili <- data.frame(x = (xmin(rst_kili) + xmax(rst_kili)) / 2, 
                         y = (ymin(rst_kili) + ymax(rst_kili)) / 2)
  coordinates(spt_kili) <- ~ x + y
  
  spplot(spy_africa, "ADMIN", colorkey = FALSE, col.regions = "transparent",
         sp.layout = list(list("sp.lines", as(spy_africa, "SpatialLines"), col = "grey50"), 
                          list("sp.lines", as(spy_tanzania, "SpatialLines"), 
                               lwd = 5), 
                          list("sp.points", spt_kili, col = "red", pch = 20, cex = 5)))
}