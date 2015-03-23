## environmental stuff

# packages
library(MODIS)

# path: modis, raw and extracted
ch_dir_arc <- "/media/fdetsch/XChange/kilimanjaro/evapotranspiration/MODIS_ARC"
ch_dir_prc <- paste0(ch_dir_arc, "/PROCESSED")

# modis options
MODISoptions(MODISserverOrder = c("LPDAAC", "LAADS"), 
             localArcPath = ch_dir_arc, outDirPath = ch_dir_prc)

# path: modis, processed
ch_dir_mod15 <- "/media/permanent/phd/scintillometer/data/mod15a2/"
ch_dir_myd15 <- "/media/permanent/phd/scintillometer/data/myd15a2/"

# path: plot coordinates
ch_dir_crd <- "/media/permanent/kilimanjaro/coordinates/coords/"
ch_fls_crd <- "PlotPoles_ARC1960_mod_20140807_final"


## data: lai-2200

ch_fls_lai <- list.files("../../phd/lai-2200", pattern = ".TXT$", 
                         full.names = TRUE)

# research sites
ch_plt <- basename(ch_fls_lai)
ls_plt <- strsplit(ch_plt, "\\.")
ch_plt <- sapply(ls_plt, "[[", 1)

# lai-2200 datasets
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
df_plt_lai <- data.frame(PlotID = ch_plt, df_lai)


## data: modis

for (product in c("MOD15A2", "MYD15A2"))
  runGdal(product, begin = "2013001", tileH = 21, tileV = 9, 
          SDSstring = "011100", job = "LAI1km", outProj = "+init=epsg:21037")
