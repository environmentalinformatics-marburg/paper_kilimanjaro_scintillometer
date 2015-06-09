## required packages and functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

## avl mai4 data
df_fls <- slsAvlFls(ch_pattern = "mrg_rf_agg10m.csv$")
df_fls_mai <- subset(df_fls, plot == "mai4")

ls_dat_mai <- lapply(1:nrow(df_fls_mai), function(i) {
  tmp_df_dat <- read.csv(df_fls_mai[i, "mrg_rf_agg10m"])
  data.frame(PlotID = df_fls_mai[i, "plot"], tmp_df_dat)
})
df_dat_mai <- do.call("rbind", ls_dat_mai)
df_dat_mai$datetime <- as.POSIXct(df_dat_mai$datetime, format = "%Y-%m-%d %H:%M:%S")

## download trmm binary files
downloadTRMM(begin = df_dat_mai$datetime[1], 
             end = df_dat_mai$datetime[nrow(df_dat_mai)], 
             dsn = "../../phd/scintillometer/data/trmm/", incl_xml = TRUE)

## list available files
ch_fls_bin <- list.files("../../phd/scintillometer/data/trmm/", 
                          full.names = TRUE, pattern = ".bin$")
ch_fls_xml <- list.files("../../phd/scintillometer/data/trmm/", 
                         full.names = TRUE, pattern = ".xml$")

## rasterize trmm binary files
ls_rst_trmm <- lapply(1:length(ch_fls_bin), function(i) {
  rst_trmm <- rasterizeTRMM(binary = ch_fls_bin[i], meta = ch_fls_xml[i])
})
rst_trmm <- stack(ls_rst_trmm)