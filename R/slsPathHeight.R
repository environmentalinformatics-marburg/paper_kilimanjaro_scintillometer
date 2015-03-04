slsPathHeight <- function(x, ...) {
  
  # locate config file
  ch_fls_cfg <-list.files(x, pattern = "^current.conf$", full.names = TRUE, 
                          recursive = TRUE)
  
  # import
  ch_cnt_cfg <- readLines(ch_fls_cfg)
  
  # entry on path length
  int_id_len <- grep("sitePathHeight", ch_cnt_cfg)
  ch_cnt_len <- ch_cnt_cfg[int_id_len]
  
  # extraction of path length
  ls_cnt_len <- strsplit(ch_cnt_len, "=")
  ch_len <- sapply(ls_cnt_len, "[[", 2)
  num_len <- as.numeric(ch_len)
  
  return(num_len)
}