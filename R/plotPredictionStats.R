plotPredictionStats <- function(reg_stats, rng = NULL, left = TRUE, ...) {
  
  # packages
  stopifnot(require(Rsenal))
  stopifnot(require(latticeExtra))
  
  # stop if supplied prediction statistics are not named
  ch_reg_stats_nms <- names(reg_stats)
  if (is.null(ch_reg_stats_nms))
    stop("Please supply named prediction performance statistics as returned by 
         Rsenal::regressionStats().")
  
  # numeric vector to data.frame
  if (is.numeric(reg_stats)) {
    mat_reg_stats <- matrix(reg_stats, 1, byrow = TRUE)
    df_reg_stats <- data.frame(mat_reg_stats)
    names(df_reg_stats) <- names(reg_stats)
  } else {
    df_reg_stats <- reg_stats
  }
  
  panel.fun <- function(...) {
    trellis.par.set("clip", list(panel = "off", strip = "off"))
    
    if (panel.number() == 1) {
      panel.axis("top", at = seq(.8, 1, .1), outside = FALSE,
                 labels = TRUE, half = FALSE, text.cex = .7, 
                 tck = .5)
      panel.axis(side = "left", at = 1, outside = TRUE, tck = .5,
                 labels = ifelse(left, expression("r"["T"]^2), FALSE), text.cex = .7)
      panel.dotplot(lwd = .5, ...)
      panel.abline(v = 1, lty = 3, lwd = 1, col = "red")
    }
    
    if (panel.number() == 2) {
      at <- pretty(rng)
      panel.axis("bottom", at = at, outside = FALSE, tck = .5,
                 labels = TRUE, half = FALSE, text.cex = .7)
      panel.axis("left", at = 2:4, outside = TRUE, text.cex = .7, tck = .5,
                 labels = if (left) {
                   c(expression("MAE"["T"]), expression("ME"["T"]), 
                     expression("RMSE"["T"]))
                   } else {
                     FALSE
                   }, half = FALSE)
      panel.abline(v = 0, lty = 3, lwd = 1, col = "red")
      panel.lines(x = x_se[c(1, 5)], col = "grey60",
                  y = y_se[c(1, 5)], lwd = 4)
      panel.lines(x = x_se[c(2, 6)], col = "grey60",
                  y = y_se[c(2, 6)], lwd = 4)
      panel.lines(x = x_se[c(3, 7)], col = "grey60",
                  y = y_se[c(3, 7)], lwd = 4)
      panel.dotplot(..., lwd = 0.5)
      panel.dotplot(x = df_plt$fit, y = df_plt$nms,
                    cex = 1, col = "grey20", lwd = 0.5, col.line = "transparent")
    }
  }
  
  nms <- names(df_reg_stats)[c(1, 3, 5)]
  nms <- c(nms, "")
  
  df_rsq <- data.frame(nms = c("Rsq", "", ""), Rsq = c(df_reg_stats$Rsq, NA, NA))
  df_rsq$nms <- factor(df_rsq$nms, levels = c("Rsq", "", "NULL"))
  
  rsq_plt <- dotplot(nms ~ Rsq, data = df_rsq, 
                     xlab = "", ylab = "", col.line = c("grey70", "transparent"),
                     col = "grey20", xlim = c(0.75, 1.05),
                     scales = list(draw = FALSE),
                     cex = 1, as.table = TRUE, 
                     par.settings = list(
                       layout.widths = list(left.padding = ifelse(left, 5, 1), right.padding = 0), 
                       layout.heights = list(top.padding = 0, bottom.padding = 0)
                     ))
  
  fit <- c(df_reg_stats$ME,
           df_reg_stats$MAE,
           df_reg_stats$RMSE, 
           NA)
  
  fit_se <- c(df_reg_stats$ME.se,
              df_reg_stats$MAE.se,
              df_reg_stats$RMSE.se, 
              NA)
  
  df_plt <- data.frame(nms = nms,
                       fit = fit,
                       fit_se = fit_se,
                       upr = fit + fit_se,
                       lwr = fit - fit_se)
  
  x_se <- c(df_plt$lwr, df_plt$upr)
  y_se <- rep(c(2, 1, 3, NA) + 1, 2)
  
  if (is.null(rng)) {
    rng <- range(x_se, na.rm = TRUE)
    rng <- round(c(rng[1] - 0.2 * rng[1],
                   rng[2] + 0.2 * rng[2]), ...)
  }
  
  err_lbl <- c("", expression("MAE"["T"]), 
               expression("ME"["T"]), expression("RMSE"["T"]))
  
  err_plt <- dotplot(nms ~ upr + lwr,
                     data = df_plt, 
                     xlab = "", ylab = "",
                     # ylim = c(-1, 4),
                     col = "grey20", col.line = c(rep("grey70", 3), "transparent"),
                     pch = "|", xlim = rng,
                     cex = 1, as.table = TRUE, 
                     scales = list(y = list(labels = err_lbl)), 
                     par.settings = list(
                       layout.widths = list(left.padding = 4, right.padding = 0), 
                       layout.heights = list(top.padding = 0, bottom.padding = 0), 
                       axis.components = list(left = list(pad1 = .5), 
                                              bottom = list(pad1 = .5), 
                                              top = list(pad1 = .5))
                     ))
  
  trellis.par.set("clip", list(panel = "off", strip = "off"))
  out_plt <- resizePanels(latticeCombineGrid(list(rsq_plt, err_plt),
                                             layout = c(1, 2)), 
                          h = c(2/6, 4/6))
  
  out_plt <- update(out_plt, panel = panel.fun)

  return(out_plt)
}
