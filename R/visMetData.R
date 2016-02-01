visMetData <- function(process_level = "mrg", 
                       path_out = "../../phd/scintillometer/out/plot_data/", 
                       ...) {
  
  #   try(detach("package:ggplot2", unload = TRUE))
  #   install.packages("inst/packages/ggplot2_1.0.1.tar.gz", repos = NULL)
  
  ## required packages and functions
  source("R/slsPkgs.R")
  source("R/slsFcts.R")
  df_sls <- slsAvlFls(...)
  
  ## sort by elevation
  id <- sapply(slsPlots(style = "elevation"), function(i) grep(i, df_sls$plot))
  df_sls <- df_sls[id, ]
  
  ## visualize meteorological time series per plot
  for (i in 1:nrow(df_sls)) {
    ## current plot and season
    ch_plot <- df_sls[i, "plot"]
    ch_ssn <- df_sls[i, "season"]
    
    ## import and reformat data
    df <- read.csv(df_sls[i, process_level])
    df <- df[, -grep("soilHeatFlux", names(df))]
    df <- df[, -grep("hour", names(df))]
    df_mlt <- melt(df, id.vars = 1)
    df_mlt$datetime <- strptime(df_mlt$datetime, format = "%Y-%m-%d %H:%M:%S")
    
    ## visualization
    p <- ggplot(aes(x = datetime, y = value), 
                data = subset(df_mlt, variable != "waterET")) + 
      geom_line(size = .7) + 
      facet_wrap(~ variable, ncol = 2, scales = "free_y") + 
      labs(x = "\nTime (hr)", y = "") + 
      theme_bw() + 
      theme(text = element_text(size = 10), axis.text = element_text(size = 7), 
            strip.text = element_text(face = "bold"))
    
    lbl <- c(expression("T"["a-200"] ~ "(" * degree * "C)"), 
             expression("T"["a-20"] ~ "(" * degree * "C)"), 
             expression("R"["dwn"] ~ "(W/m"^2 * ")"), 
             expression("R"["up"] ~ "(W/m"^2 * ")"), 
             "rH (%)", "p (Pa)", "VPD", "Rainfall (mm)")
    
    p_upd <- facet_wrap_labeller(p, lbl)
    
    # ch_fls_out <- paste(ch_plot, ch_ssn, process_level, sep = "_")
    ch_fls_out <- paste0("figure_a-", formatC(i, width = 2, flag = 0), ".png")
    ch_fls_out <- paste0(path_out, "/", ch_fls_out)
    png(ch_fls_out, width = 20, height = 16, units = "cm", res = 500)
    grid.newpage()
    
    vp_fig <- viewport(x = 0, y = .1, just = c("left", "bottom"), 
                       width = 1, height = .9)
    pushViewport(vp_fig)
    print(p_upd)
    
    upViewport(0)
    vp_cap <- viewport(x = .5, y = .07, width = 1, height = .1)
    pushViewport(vp_cap)
    grid.text(bquote(atop(bold("Figure A-") * bold(.(i)) * ". Meteorological data from" ~ .(ch_plot) * ".")), 
              gp = gpar(cex = .8))
    
    dev.off()
  }
  
  return(invisible())
}