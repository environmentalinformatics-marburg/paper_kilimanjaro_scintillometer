visRainfall <- function(process_level = "mrg_rf_agg01h", 
                        path_out = "../../phd/scintillometer/out/plot_data/", 
                        ...) {
  
  ## required packages and functions
  source("R/slsPkgs.R")
  source("R/slsFcts.R")
  df_sls <- slsAvlFls(...)
  
  ## merge hourly rainfall per plot
  ls_rain <- lapply(1:nrow(df_sls), function(i) {
    ## current plot and season
    ch_plot <- df_sls[i, "plot"]
    ch_ssn <- df_sls[i, "season"]
    
    ## import and reformat data
    df <- read.csv(df_sls[i, process_level])
    df <- df[, c("datetime", "precipRate")]
    df$datetime <- strptime(df$datetime, format = "%Y-%m-%d %H:%M:%S")
    
    data.frame(plot = ch_plot, season = ch_ssn, df)
  })
  df_rain <- do.call("rbind", ls_rain)
  
  ## reorder factor levels
  df_rain$plot <- factor(df_rain$plot, levels = slsPlots(style = "ggplot"))
  
  ## visualization
  p <- ggplot(aes(x = datetime, y = precipRate), data = df_rain) + 
    geom_hline(aes(yintercept = 0), colour = "grey75") + 
    geom_histogram(stat = "identity") + 
    facet_wrap(~ plot, ncol = 2, scales = "free", drop = FALSE) + 
    labs(x = "\nTime (hours)", y = "Rainfall (mm)\n") + 
    scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%d.%m.")) + 
    theme_bw() + 
    theme(text = element_text(size = 14), axis.text = element_text(size = 12))
  
  ch_fls_out <- paste0("rainfall_", process_level)
  ch_fls_out <- paste0(path_out, "/", ch_fls_out, ".png")
  png(ch_fls_out, width = 30, height = 30, units = "cm", res = 300, 
      pointsize = 18)
  print(p)
  dev.off()

  return(invisible())
}