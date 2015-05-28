visMetData <- function(process_level = "mrg", 
                       path_out = "../../phd/scintillometer/out/plot_data/", 
                       ...) {
  
  ## required packages and functions
  source("R/slsPkgs.R")
  source("R/slsFcts.R")
  df_sls <- slsAvlFls(...)
  
  ## visualize meteorological time series per plot
  for (i in 1:nrow(df_sls)) {
    ## current plot and season
    ch_plot <- df_sls[i, "plot"]
    ch_ssn <- df_sls[i, "season"]
    
    ## import and reformat data
    df <- read.csv(df_sls[i, process_level])
    df_mlt <- melt(df, id.vars = 1)
    df_mlt$datetime <- strptime(df_mlt$datetime, format = "%Y-%m-%d %H:%M:%S")
    
    ## visualization
    p <- ggplot(aes(x = datetime, y = value), 
                data = subset(df_mlt, variable != "waterET")) + 
      geom_line() + 
      geom_point(size = .1) + 
      facet_wrap(~ variable, ncol = 2, scales = "free_y") + 
      labs(title = paste0(ch_plot, "\n"), x = "\nTime (min)", y = "") + 
      theme_bw() + 
      theme(text = element_text(size = 14), axis.text = element_text(size = 12))
    
    ch_fls_out <- paste(ch_plot, ch_ssn, process_level, sep = "_")
    ch_fls_out <- paste0(path_out, "/", ch_fls_out, ".png")
    png(ch_fls_out, width = 30, height = 30, units = "cm", res = 300, 
        pointsize = 18)
    print(p)
    dev.off()
  }
  
  return(invisible())
}