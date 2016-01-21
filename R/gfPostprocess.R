gfPostprocess <- function(wsp, dsn_regstats = "data/regstats_", 
                          dsn_varimp = "data/varimp_", left = TRUE) {
  
  tmp_ls_plt <- strsplit(basename(wsp), "_")
  tmp_ch_plt <- sapply(tmp_ls_plt, "[[", 3)
  tmp_ch_dt <- sapply(tmp_ls_plt, "[[", 4)
  
  # cv/prediction statistics
  ch_fls_eval <- paste0(dsn_regstats, tmp_ch_plt, "_", tmp_ch_dt, ".csv")
  tmp_df_rf_eval <- read.csv(ch_fls_eval)
  
  # variable importances
  ch_fls_varimp <- paste0(dsn_varimp, tmp_ch_plt, "_", tmp_ch_dt, ".csv")
  tmp_df_rf_varimp <- read.csv(ch_fls_varimp)
  
  # plot prediction stats
  num_reg_stats <- colMeans(tmp_df_rf_eval[, 6:ncol(tmp_df_rf_eval)])
  p_reg_stats <- plotPredictionStats(num_reg_stats, digits = 2, 
                                     rng = c(-0.012, 0.07), left = left)

  # modal mtry and mean training and prediction scores
  int_mdl_mtry <- modal(tmp_df_rf_eval$mtry)
  num_mu_scores <- colMeans(tmp_df_rf_eval[, 3:ncol(tmp_df_rf_eval)])
  mat_mu_scores <- matrix(num_mu_scores, 1, byrow = TRUE)
  df_mu_scores <- data.frame(mat_mu_scores)
  names(df_mu_scores) <- names(num_mu_scores)
  
  # return data frame and corresponding plot
  list(data.frame(plot = tmp_ch_plt, mtry = int_mdl_mtry, df_mu_scores), 
       data.frame(plot = tmp_ch_plt, mtry = int_mdl_mtry, tmp_df_rf_varimp),
       p_reg_stats)
}
