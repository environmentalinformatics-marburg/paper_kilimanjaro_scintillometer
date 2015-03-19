# packages
library(reshape2)
library(latticeExtra)

# channel intensities
ch_channels <- c("<XA", "<YA", "<XB", "<YB")
id_channels <- sapply(ch_channels, function(i) grep(i, names(tmp_df)))
df_channels <- tmp_df[, c(1, id_channels)]
df_channels$datetime <- as.POSIXct(df_channels$datetime, 
                                   format = "%Y-%m-%d %H:%M:%S")

# # channel ranges
# mat_rng_channels <- sapply(id_channels, function(i) {
#   range(tmp_df[, i], na.rm = TRUE)
# })

# error flag == 32 (SignalReduced)
df_error <- tmp_df[, c(1, ncol(tmp_df))]
df_error$datetime <- as.POSIXct(df_error$datetime, 
                                   format = "%Y-%m-%d %H:%M:%S")
df_error_32 <- subset(df_error, error == 32) 

# reformat data
df_channels_mlt <- melt(df_channels, id.vars = 1, variable.name = "channel")

# visualization of raw channel data including phases with reduced signal
xyplot(value ~ datetime | channel, data = df_channels_mlt, layout = c(1, 4), 
       panel = function(x, y) {
         panel.abline(v = df_error_32$datetime, col = "coral", 
                      alpha = .05, lwd = 2)
         panel.xyplot(x, y, type = "l", col = "grey35")
       })

# rejection of measurements with reduced signal
int_id_err32 <- which(df_error$error == 32)
df_channels_err32 <- df_channels
df_channels_err32[int_id_err32, ] <- NA

# reformat data
df_channels_err32_mlt <- melt(df_channels_err32, id.vars = 1, 
                              variable.name = "channel")

# visualization
xyplot(value ~ datetime | channel, data = df_channels_mlt, 
       layout = c(1, 4), panel = function(x, y) {
         panel.xyplot(x, y, type = "l", col = "grey75")
       }) + 
  as.layer(xyplot(value ~ datetime | channel, data = df_channels_err32_mlt, 
                  layout = c(1, 4), panel = function(x, y) {
                    panel.xyplot(x, y, type = "l", col = "grey25")
                  }))

