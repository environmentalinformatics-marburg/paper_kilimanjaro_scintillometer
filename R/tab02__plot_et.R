# packages
library(TSdist)

# sls plots
ch_sls_plt <- c("sav0", "sav5", "mai0", "mai4", 
                "gra1", "gra2", "cof3", "cof2", 
                "fer0", "fed1", "hel1")

# 20-min data
ls_sls_dv_20m <- lapply(1:nrow(df_sls_fls_rs), function(i) {
  tmp_df <- slsDiurnalVariation(fn = df_sls_fls_rs$mrg[i], agg_by = 20, 
                                FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls_rs$plot[i], habitat = df_sls_fls_rs$habitat[i],
             season = df_sls_fls_rs$season[i], tmp_df, stringsAsFactors = FALSE)
})
df_sls_dv_20m <- do.call("rbind", ls_sls_dv_20m)

# subset by land-cover type
ch_sls_lct <- unique(df_sls_dv_20m$habitat)

ls_sls_scores <- lapply(ch_sls_lct, function(i) {
  tmp_df <- subset(df_sls_dv_20m, habitat == i)
  tmp_ls <- split(tmp_df, tmp_df$plot)
  
  if (length(tmp_ls) > 1) {
    tmp_x <- tmp_ls[[1]]$waterET
    tmp_y <- tmp_ls[[2]]$waterET

    # r-squared
    tmp_lm <- lm(tmp_y ~ tmp_x)
    tmp_rsq <- summary(tmp_lm)$r.squared
    tmp_p <- summary(tmp_lm)$coefficients[2, 4]
    
    # dissimilarity
    log_x_isna <- is.na(tmp_x)
    log_y_isna <- is.na(tmp_y)
    log_isna <- log_x_isna | log_y_isna
    if (any(log_isna)) {
      tmp_x_nona <- tmp_x[-which(log_isna)]
      tmp_y_nona <- tmp_y[-which(log_isna)]
    } else {
      tmp_x_nona <- tmp_x
      tmp_y_nona <- tmp_y
    }
    tmp_edist <- euclideanDistance(tmp_x_nona, tmp_y_nona)
    
    tmp_df_scores <- data.frame(habitat = i, 
                                rsq = tmp_rsq, p = tmp_p, edist = tmp_edist)
  } else {
    tmp_df_scores <- data.frame(habitat = i, 
                                rsq = NA, p = NA, edist = NA)
  }
  
  return(tmp_df_scores)
})
df_sls_scores <- do.call("rbind", ls_sls_scores)
