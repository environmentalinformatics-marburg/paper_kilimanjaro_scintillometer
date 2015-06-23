visVarQuantiles <- function(param = "pressure", luc = c("sav", "mai"), 
                            ssn = "r") {
  
  ## required packages and functions
  source("R/slsPkgs.R")
  source("R/slsFcts.R")
  
  ## available datasets
  df_sls <- slsAvlFls(ssn = ssn)
  
  ## et during lowest (<25% quant) and highest variable values (>75%)
  df_luc <- subset(df_sls, habitat %in% luc)
  
  ls_et_var <- lapply(1:nrow(df_luc), function(i) {
    tmp_ch_plot <- df_luc$plot[i]
    tmp_df_in <- read.csv(df_luc$mrg_rf_agg01h[i])
    
    tmp_num_quan <- quantile(tmp_df_in[, param], na.rm = TRUE)
    tmp_mat_quan <- matrix(tmp_num_quan, nrow = 1)
    
    # lower quantile
    tmp_df_low <- subset(tmp_df_in, tmp_df_in[, param] < tmp_num_quan[2])
    tmp_df_low$class <- "<25%"
    
    #   # between lower and upper
    #   tmp_df_med <- subset(tmp_df_in, pressure > tmp_num_quan[2] & 
    #                          pressure < tmp_num_quan[4])
    #   tmp_df_med$class <- "25-75%"
    
    # upper quantile
    tmp_df_high <- subset(tmp_df_in, tmp_df_in[, param] > tmp_num_quan[4])
    tmp_df_high$class <- ">75%"
    
    tmp_df_mrg <- rbind(tmp_df_low, tmp_df_high)
    tmp_df_mrg$plot <- tmp_ch_plot
    return(tmp_df_mrg)
  })
  df_et_var <- do.call("rbind", ls_et_var)
  df_et_var$plot <- factor(df_et_var$plot, levels = slsPlots())
  
  ## visualize et
  p_et <- ggplot(aes(x = plot, y = waterET, fill = class), data = df_et_var) + 
    geom_boxplot(notch = FALSE) + 
    labs(x = "", y = "Evapotranspiration (mm/h)\n") + 
    theme_bw()
  
  ## visualize frequency per hour of day
  ch_datetime <- as.character(df_et_var$datetime)
  px_datetime <- strptime(ch_datetime, format = "%Y-%m-%d %H:%M:%S")
  df_et_var$hour <- hour(px_datetime)
  
  p_datetime <- ggplot(aes(x = hour, group = class, fill = class), data = df_et_var) + 
    facet_wrap(~ plot, ncol = 2) + 
    geom_histogram(binwidth = 1, position = "dodge") + 
    labs(x = "\nHour of day", y = "Count\n") + 
    theme_bw()
  
  ## return combined plot
  return(grid.arrange(p_et, p_datetime))
}

visVarQuantiles(luc = c("fer", "fed", "hel"))

foreach(luc = list(c("sav", "mai"), c("gra", "cof"), c("fer", "fed", "hel"))) %do%
  visVarQuantiles(luc = luc, param = "vpd")
