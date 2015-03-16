# output storage path
ch_dir_out_agg01d <- "../../phd/scintillometer/data/agg01d/"

# et and gpp data
load(paste0(ch_dir_out_agg01d, "df_sls_dv_01d.RData"))
load(paste0(ch_dir_out_agg01d, "df_sls_gpp_md.RData"))

# merge and calculate water-use efficiency
df_sls_gpp <- merge(df_sls_dv_01d, df_sls_gpp_md, by = 1:2, all = TRUE)
df_sls_gpp$wue <- df_sls_gpp$gpp / df_sls_gpp$waterET
