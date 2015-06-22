ch_rs <- paste0("data/regstats", c("_", "_vpd_", "_vpd_agg10m_"))
ch_vi <- paste0("data/varimp", c("_", "_vpd_", "_vpd_agg10m_"))
foreach(f = ch_rs, g = ch_vi) %do% {
  
  ls_rf_scores_whr <- lapply(srunWorkspaces, function(h) {
    gfPostprocess(h, dsn_regstats = f, dsn_varimp = g)
  })
  
  # wet season data only
  int_id_dryssn <- c(11, 13)
  ls_rf_scores_dryssn <- ls_rf_scores_whr[-int_id_dryssn]
  
  # split data into cv/prediction statistics...
  ls_rf_scores_dryssn_stats <- lapply(ls_rf_scores_dryssn, function(i) i[[1]])
  df_rf_scores_stats <- do.call("rbind", ls_rf_scores_dryssn_stats)
  # ...variable importances...
  ls_rf_scores_dryssn_varimp <- lapply(ls_rf_scores_dryssn, function(i) i[[2]])
  df_rf_scores_dryssn_varimp <- do.call("rbind", ls_rf_scores_dryssn_varimp)
  # ...and referring visualization
  ls_rf_scores_dryssn_vis <- lapply(ls_rf_scores_dryssn, function(i) i[[3]])
  
  # plotting order
  ch_sls_plt <- slsPlots()
  
  fc_rf_plt <- df_rf_scores_stats$plot
  ch_rf_plt <- as.character(fc_rf_plt)
  
  int_id_plt <- match(ch_rf_plt, ch_sls_plt)
  ls_rf_scores_dryssn_vis <- ls_rf_scores_dryssn_vis[int_id_plt]
  
  # add plot names to figures
  ls_rf_scores_dryssn_vis <- lapply(seq(ls_rf_scores_dryssn_vis), function(i) {
    update(ls_rf_scores_dryssn_vis[[i]], main = ch_sls_plt[i])
  })
  
  # grid.arrange
  do.call(function(...) grid.arrange(..., ncol = 4, as.table = TRUE), 
          ls_rf_scores_dryssn_vis)
  
}