plotPredictionStats <- function(reg_stats, ...) {
  
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
    mat_reg_stats <- matrix(num_reg_stats, 1, byrow = TRUE)
    df_reg_stats <- data.frame(mat_reg_stats)
    names(df_reg_stats) <- names(num_reg_stats)
  } else {
    df_reg_stats <- reg_stats
  }
  
  panel.fun <- function(...) {
    if (panel.number() == 1) { 
      at<-pretty(c(0, 1))
      panel.axis("top", at = at, outside = FALSE,
                 labels = TRUE, half = FALSE)
      panel.abline(v = 1, lty = 3, lwd = 1)
      panel.dotplot(..., lwd = 0.5)
    }
    if (panel.number() == 2) {
      at <- pretty(rng)
      panel.axis("bottom", at = at, outside = FALSE,
                 labels = TRUE, half = FALSE)
      panel.abline(v = 0, lty = 3, lwd = 1)
      panel.lines(x = x_se[c(1, 4)], col = "grey60",
                  y = y_se[c(1, 4)], lwd = 4)
      panel.lines(x = x_se[c(2, 5)], col = "grey60",
                  y = y_se[c(2, 5)], lwd = 4)
      panel.lines(x = x_se[c(3, 6)], col = "grey60",
                  y = y_se[c(3, 6)], lwd = 4)
      panel.dotplot(..., lwd = 0.5)
      panel.dotplot(x = df_plt$fit, y = df_plt$nms,
                    cex = 1.3, col = "grey20", lwd = 0.5)
    }
  }
  
  nms <- names(df_reg_stats)[c(1, 3, 5)]
  
  rsq_plt <- dotplot("Rsq" ~ Rsq, data = df_reg_stats, 
                     xlab = "", ylab = "",
                     col = "grey20", xlim = c(-0.05, 1.05),
                     scales = list(x = list(draw = FALSE)),
                     par.settings = envinmr.theme(),
                     cex = 1.2, as.table = TRUE)
  
  fit <- c(df_reg_stats$ME,
           df_reg_stats$MAE,
           df_reg_stats$RMSE)
  
  fit_se <- c(df_reg_stats$ME.se,
              df_reg_stats$MAE.se,
              df_reg_stats$RMSE.se)
  
  df_plt <- data.frame(nms = nms,
                       fit = fit,
                       fit_se = fit_se,
                       upr = fit + fit_se,
                       lwr = fit - fit_se)
  
  x_se <- c(df_plt$lwr, df_plt$upr)
  y_se <- rep(c(2, 1, 3), 2)
  
  rng <- range(x_se)
  rng <- round(c(rng[1] - 0.2 * rng[1],
                 rng[2] + 0.2 * rng[2]), ...)
  
  err_plt <- dotplot(nms ~ upr + lwr,
                     data = df_plt, , 
                     xlab = "", ylab = "",
                     ylim = c(-1, 4),
                     col = "grey20",
                     pch = "|",
                     par.settings = envinmr.theme(),
                     cex = 1.2, as.table = TRUE)
  
  out_plt <- resizePanels(latticeCombineGrid(list(rsq_plt, err_plt),
                                             layout = c(1, 2)), 
                          h = c(1/4, 3/4))
  
  out_plt <- update(out_plt, 
                    scales = list(y = list(at = 1:3, labels = sort(nms))))
  
  out_plt <- update(out_plt, panel = panel.fun)
  return(out_plt)
}
