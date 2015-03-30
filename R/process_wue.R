# output storage path
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"

# 60-min et rates (mm/h)
df_sls_fls <- slsAvlFls()
ls_sls_dv_01h <- lapply(1:nrow(df_sls_fls), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls$mrg_rf[i], agg_by = 60, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls$plot[i], habitat = df_sls_fls$habitat[i],
             season = df_sls_fls$season[i], tmp_df)
})
df_sls_dv_01h <- do.call("rbind", ls_sls_dv_01h)

# diurnal et amounts (mm)
ls_sls_dv_01d <- lapply(ls_sls_dv_01h, function(i) {
  tmp.df <- slsAggregate(fn = i, agg_by = 24, include_time = FALSE,
                         FUN = function(...) round(sum(..., na.rm = TRUE), 1))
  data.frame(plot = unique(i$plot), habitat = unique(i$habitat), 
             season = unique(i$season), tmp.df)
})
df_sls_dv_01d <- do.call("rbind", ls_sls_dv_01d)

# et and gpp data
# load(paste0(ch_dir_out_agg01d, "df_sls_dv_01d.RData"))
load(paste0(ch_dir_out_agg01d, "df_sls_gpp_md.RData"))

# merge and calculate water-use efficiency
df_sls_gpp <- merge(df_sls_dv_01d, df_sls_gpp_md, by = c("plot", "season"), all = TRUE)
df_sls_gpp$wue <- df_sls_gpp$gpp / df_sls_gpp$waterET
