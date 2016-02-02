visRainfall <- function(process_level = "mrg_rf_agg01h", 
                        path_out = "../../phd/scintillometer/out/plot_data/", 
                        col = "precipRate", return_data = FALSE, visualize = TRUE, ...) {
  
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
    df <- df[, c("datetime", col)]
    names(df)[2] <- "variable"
    df$datetime <- strptime(df$datetime, format = "%Y-%m-%d %H:%M:%S")
    
    data.frame(plot = ch_plot, season = ch_ssn, df)
  })
  df_rain <- do.call("rbind", ls_rain)
  
  ## reorder factor levels
  df_rain$plot <- factor(df_rain$plot, 
                         levels = slsPlots(style = "ggplot", empty = 2L))
  
  ## visualization
  if (visualize) {

    df_rain$hour <- factor(hour(df_rain$datetime), levels = 0:23)
    
    box_settings <- list(box.umbrella = list(lty = 1, col = "black"), 
                         box.rectangle = list(col = "black", fill = "grey75"), 
                         strip.background = list(col = "grey90"), 
                         add.text = list(font = 2, cex = .85))
    
    lbl <- c(rbind(seq(0, 22, 2), rep("", 12)))
    
    p <- bwplot(variable ~ hour | plot, data = df_rain, pch = "|", 
                par.settings = box_settings, as.table = TRUE, layout = c(2, 6),
                panel = function(...) {
                  panel.bwplot(..., coef = 0)
                }, drop.unused.levels = FALSE, ylim = c(-1, 20.75),
                scales = list(draw = TRUE, y = list(cex = .7),
                              x = list(at = 1:24, labels = lbl, cex = .7)), 
                xlab = list("Hour of day", cex = .85), 
                ylab = list("Rainfall (mm)", cex = .85))
    
    ## in-text png version
    # ch_fls_out <- paste(col, process_level, sep = "_")
    ch_fls_out <- "figure-a01"
    ch_fls_png <- paste0(path_out, "/", ch_fls_out, ".png")
    png(ch_fls_png, width = 20, height = 22, units = "cm", res = 500)
    grid.newpage()
    print(p, newpage = FALSE)
    
    downViewport(trellis.vpname("figure"))
    vp_mask <- viewport(x = .5 + .00075, y = 5/6 + .00075, width = .51, 
                        height = .17, just = c("left", "bottom"))
    pushViewport(vp_mask)
    grid.rect(gp = gpar(col = "transparent"))
    
    dev.off()
    
    ## stand-alone eps version
    ch_fls_eps <- paste0(path_out, "/", ch_fls_out, ".eps")
    setEPS()
    postscript(ch_fls_eps, width = 20*.3937, height = 22*.3937)
    grid.newpage()
    print(p, newpage = FALSE)
    
    downViewport(trellis.vpname("figure"))
    vp_mask <- viewport(x = .5 + .00075, y = 5/6 + .00075, width = .51, 
                        height = .17, just = c("left", "bottom"))
    pushViewport(vp_mask)
    grid.rect(gp = gpar(col = "transparent"))
    
    dev.off()
  }
  
  ## return data (optional)
  if (return_data) 
    return(df_rain)
  else 
    return(invisible())
}