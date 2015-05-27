slsAvlFls <- function(ch_path = "../../phd/scintillometer/data", 
                      ch_pattern = c("mrg.csv$", "mrg_rf.csv$", "mrg_rf_agg10m.csv$", 
                                     "mrg_rf_agg01h.csv$"), 
                      ssn = NULL, disturbed = NULL, ...) {
  
  ls_df_sls_fls <- lapply(ch_pattern, function(tmp_ch_pattern) {
    tmp_ch_fls <- list.files(ch_path, full.names = TRUE,
                             pattern = tmp_ch_pattern, recursive = TRUE)
    
    # current plot
    ch_sls_plt <- basename(tmp_ch_fls)
    ls_sls_plt <- strsplit(ch_sls_plt, "_")
    ch_sls_plt <- sapply(ls_sls_plt, "[[", 1)
    
    ch_sls_hab <- substr(ch_sls_plt, 1, 3)
    
    # current season
    int_sls_ds <- which(duplicated(ch_sls_plt))
    ch_sls_ssn <- rep("r", length(ch_sls_plt))
    ch_sls_ssn[int_sls_ds] <- "d"
    
    ch_process_level <- substr(tmp_ch_pattern, 1, nchar(tmp_ch_pattern)-5)
    tmp_df_fls <- data.frame(ch_sls_plt, ch_sls_hab, ch_sls_ssn, tmp_ch_fls, 
                             stringsAsFactors = FALSE)
    names(tmp_df_fls) <- c("plot", "habitat", "season", ch_process_level)
    return(tmp_df_fls)
  })
  
  df_sls_fls <- Reduce(function(...) merge(..., by = c(1, 2, 3), sort = FALSE), 
                       ls_df_sls_fls)
  
  # seasonal subset (optional)
  if (!is.null(ssn))
    df_sls_fls <- subset(df_sls_fls, season == ssn)
  
  # degree of disturbance subset (optional)
  if (!is.null(disturbed)) {
    if (!disturbed)
      df_sls_fls <- subset(df_sls_fls, habitat %in% c("sav", "gra", "fer", "hel"))
    else 
      df_sls_fls <- subset(df_sls_fls, habitat %in% c("mai", "cof", "fed"))
  }
  
  return(df_sls_fls)
}