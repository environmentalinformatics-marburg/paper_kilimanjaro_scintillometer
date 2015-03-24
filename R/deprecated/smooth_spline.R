ls_sls_01h <- lapply(1:nrow(df_sls_fls_rs), function(i) {
  tmp_df <- slsAggregate(fn = df_sls_fls_rs$mrg_rf[i], include_time = TRUE, 
                         agg_by = 60, FUN = function(...) mean(..., na.rm = TRUE))
  data.frame(plot = df_sls_fls_rs$plot[i], habitat = df_sls_fls_rs$habitat[i],
             season = df_sls_fls_rs$season[i], tmp_df)
})
df_sls_01h <- do.call("rbind", ls_sls_01h)

# smoothing spline
tst <- ls_sls_01h[[1]]
tst <- na.omit(tst)
tst$id <- as.integer(tst$time)
tst_ss <- smooth.spline(x = tst$id, y = tst$waterET, spar = .01)

ch_rsp <- paste0(formatC(0:23, width = 2, flag = 0), ":00:00")
fc_rsp <- factor(ch_rsp, levels = levels(tst$time))
int_rsp <- as.integer(fc_rsp)
ls_prd <- predict(tst_ss, int_rsp)

ggplot(aes(x = datetime, y = waterET), data = df_sls_dv_01h) + 
  geom_histogram(stat = "identity", position = "dodge", 
                 ,subset = .(plot == "fed1")) + 
  # geom_line(aes(x = ls_prd$x, y = ls_prd$y)) + 
  stat_smooth(aes(x = time, y = waterET), data = tst, 
              method = "lm", formula = y ~ ns(x, 3))
  
ggplot(aes(x = time, y = waterET, group = 1), data = tst) + 
  geom_histogram(aes(x = datetime, y = waterET), data = df_sls_dv_01h, 
                 stat = "identity", position = "dodge", ,subset = .(plot == "fed1")) + 
  stat_smooth(method = "loess", span = .75) 
  
