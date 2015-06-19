summarizeVar <- function(files, param = "waterET", 
                         FUN_hr = function(...) mean(..., na.rm = TRUE), 
                         FUN_dy = function(...) sum(..., na.rm = TRUE),
                         file_out = NULL) {
  
  ## required packages
  stopifnot(require(lubridate))
  stopifnot(require(plotrix))
  stopifnot(require(dplyr))
  
  ## loop over files
  ls_var <- lapply(files, function(i) {
    
    # current plot
    ch_plot <- substr(basename(i), 1, 4)
    
    # import, reformat and merge daily data
    df_var <- read.csv(i, stringsAsFactors = FALSE)
    df_var <- df_var[, c("datetime", param)]
    
    ls_time <- strsplit(df_var$datetime, " ")
    ch_time <- sapply(ls_time, "[[", 2)
    df_var$time <- ch_time
    
    ls_var <- split(df_var, yday(df_var$datetime))
    df_var_mrg <- suppressWarnings(
      Reduce(function(...) merge(..., by = "time"), ls_var)
    )
    df_var_mrg <- df_var_mrg[, -grep("datetime", names(df_var_mrg))]
    
    # hourly values (fun) and standard errors (se)
    num_fun <- apply(df_var_mrg[, 2:ncol(df_var_mrg)], 1, FUN = FUN_hr)
    num_se <- apply(df_var_mrg[, 2:ncol(df_var_mrg)], 1, 
                    FUN = function(...) std.error(..., na.rm = TRUE))
    
    ## merge and return data
    data.frame(PlotID = ch_plot, time = df_var_mrg$time, 
               varfun = num_fun, varse = num_se)
  })
  df_var <- do.call("rbind", ls_var)
  
  ## maximum hourly and mean daily values
  df_var %>%
    dplyr::select(PlotID, time, varfun) %>%
    group_by(PlotID) %>% 
    filter(varfun == max(varfun)) %>%
    mutate(varmax = round(varfun, 2)) %>%
    dplyr::select(PlotID, varmax, time) %>%
    data.frame() %>%
    arrange(desc(varmax)) -> df_max_hr
  
  df_var %>%
    dplyr::select(PlotID, time, varfun) %>%
    group_by(PlotID) %>% 
    summarise(varfun = FUN_dy(varfun)) %>%
    data.frame() %>%
    arrange(desc(varfun)) -> df_max_dy
  
  df_all <- merge(df_max_dy, df_max_hr, by = "PlotID", sort = FALSE)
  names(df_all) <- gsub("var", param, names(df_all))
  
  if (!is.null(file_out))
    write.csv(df_all, file_out)
  
  return(df_all)
}