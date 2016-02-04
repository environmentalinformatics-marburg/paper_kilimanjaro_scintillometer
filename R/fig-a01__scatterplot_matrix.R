### environmental stuff --------------------------------------------------------

## clear workspace
rm(list = ls(all = TRUE))

## required package
library(car)

## required functions
source("R/slsPkgs.R")
source("R/slsFcts.R")

ch_dir_pub <- "../../publications/paper/detsch_et_al__spotty_evapotranspiration/"
ch_dir_tbl <- "../../phd/scintillometer/data/tbl/"

## input data
df_fls <- slsAvlFls(ssn = "r")

## ta
df_ta <- summarizeVar(df_fls$mrg_rf_agg01h, param = "tempUp", 
                      FUN_dy = function(...) mean(..., na.rm = TRUE),
                      file_out = paste0(ch_dir_tbl, "table_ta.csv"))

## rh
df_rh <- summarizeVar(df_fls$mrg_rf_agg01h, param = "humidity", 
                      FUN_dy = function(...) mean(..., na.rm = TRUE),
                      file_out = paste0(ch_dir_tbl, "table_rh.csv"))

## radiation
df_rad <- summarizeVar(df_fls$mrg_rf_agg01h, param = "dwnRad", 
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_rad.csv"))

## et
df_et <- summarizeVar(df_fls$mrg_rf_agg01h, 
                      file_out = paste0(ch_dir_tbl, "table_et.csv"))

## vpd
df_vpd <- summarizeVar(df_fls$mrg_rf_agg01h, param = "vpd", 
                       FUN_dy = function(...) mean(..., na.rm = TRUE),
                       file_out = paste0(ch_dir_tbl, "table_vpd.csv"))

## merge data
df_var <- Reduce(function(...) merge(..., by = "PlotID"), 
                 list(df_ta, df_rh, df_vpd, df_rad, df_et))

## labels
lbl <- c(expression("\nT"["a-150"] ~ "(" * degree * "C)"), "\nrH (%)", 
         "\nVPD (Pa)", expression("\nR"["dwn"] ~ "(W/m"^2 * ")"), "\nET (mm)")

## r-squared and p values
lst_mod <- lapply(c(2, 5, 8, 11), function(i) {
  lst <- lapply(seq(i+3, 14, 3), function(j) { 
    mod <- lm(df_var[, j] ~ df_var[, i])
    data.frame(pred = names(df_var)[i], resp = names(df_var)[j], 
               rsq = summary(mod)$r.squared, p = coef(summary(mod))[2, 4])
  })
  
  do.call("rbind", lst)
})

## reformat model coefficients
dat_mod <- do.call("rbind", lst_mod)
dat_mod[, 3] <- round(dat_mod[, 3], 2)

id_sig <- dat_mod[, 4] < 0.001
dat_mod[, 4] <- paste("p =", round(dat_mod[, 4], 3))
dat_mod[id_sig, 4] <- "p < 0.001"

## in-text png version
png(paste0(ch_dir_pub, "fig/figure-a01.png"), width = 20, height = 16, 
    units = "cm", res = 500)
print(spm(df_var[, c(2, 5, 8, 11, 14)], smooth = FALSE, 
          diagonal = "none", col = c("red", "black", "grey60"), lty = 2,
          var.labels = lbl, cex.labels = 1.5))

# temperature
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[1, 3]) * "," ~ .(dat_mod[1, 4])), 
          gp = gpar(cex = .7), x = .327, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[2, 3]) * "," ~ .(dat_mod[2, 4])), 
          gp = gpar(cex = .7), x = .5, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[3, 3]) * "," ~ .(dat_mod[3, 4])), 
          gp = gpar(cex = .7), x = .675, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[4, 3]) * "," ~ .(dat_mod[4, 4])), 
          gp = gpar(cex = .7), x = .845, y = .774)

# humidity
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[5, 3]) * "," ~ .(dat_mod[5, 4])), 
          gp = gpar(cex = .7), x = .5, y = .608)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[6, 3]) * "," ~ .(dat_mod[6, 4])), 
          gp = gpar(cex = .7), x = .675, y = .608)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[7, 3]) * "," ~ .(dat_mod[7, 4])), 
          gp = gpar(cex = .7), x = .845, y = .608)

# vpd
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[8, 3]) * "," ~ .(dat_mod[8, 4])), 
          gp = gpar(cex = .7), x = .675, y = .441)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[9, 3]) * "," ~ .(dat_mod[9, 4])), 
          gp = gpar(cex = .7), x = .845, y = .441)

# rdwn
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[10, 3]) * "," ~ .(dat_mod[10, 4])), 
          gp = gpar(cex = .7), x = .845, y = .2745)

dev.off()

## stand-alone eps version
setEPS()
postscript(paste0(ch_dir_pub, "fig/figure-a01.eps"), 
           width = 20*.3937, height = 16*.3937)

print(spm(df_var[, c(2, 5, 8, 11, 14)], smooth = FALSE, 
          diagonal = "none", col = c("red", "black", "grey60"), lty = 2,
          var.labels = lbl, cex.labels = 1.5))

# temperature
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[1, 3]) * "," ~ .(dat_mod[1, 4])), 
          gp = gpar(cex = .7), x = .327, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[2, 3]) * "," ~ .(dat_mod[2, 4])), 
          gp = gpar(cex = .7), x = .5, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[3, 3]) * "," ~ .(dat_mod[3, 4])), 
          gp = gpar(cex = .7), x = .675, y = .774)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[4, 3]) * "," ~ .(dat_mod[4, 4])), 
          gp = gpar(cex = .7), x = .845, y = .774)

# humidity
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[5, 3]) * "," ~ .(dat_mod[5, 4])), 
          gp = gpar(cex = .7), x = .5, y = .608)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[6, 3]) * "," ~ .(dat_mod[6, 4])), 
          gp = gpar(cex = .7), x = .675, y = .608)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[7, 3]) * "," ~ .(dat_mod[7, 4])), 
          gp = gpar(cex = .7), x = .845, y = .608)

# vpd
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[8, 3]) * "," ~ .(dat_mod[8, 4])), 
          gp = gpar(cex = .7), x = .675, y = .441)

grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[9, 3]) * "," ~ .(dat_mod[9, 4])), 
          gp = gpar(cex = .7), x = .845, y = .441)

# rdwn
grid.text(bquote("r"^2 ~ "=" ~ .(dat_mod[10, 3]) * "," ~ .(dat_mod[10, 4])), 
          gp = gpar(cex = .7), x = .845, y = .2745)

dev.off()
