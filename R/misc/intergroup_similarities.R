cl <- makeCluster(4)
registerDoParallel(cl)

ch_plotid <- sapply(ls_all, function(i) unique(i$PlotID))

## reorder list entries 
int_order <- sapply(slsPlots(style = "elevation"), function(i) {
  which(i == ch_plotid)
})
ls_all <- ls_all[int_order]
ch_plotid <- ch_plotid[int_order]

ls_stats <- foreach(i = ls_all, .packages = "TSdist") %dopar% {
  
  # y values
  y <- i$ETmu
  plot_y <- unique(i$PlotID)
  
  ls_stats <- lapply(ls_all, function(j) {
    
    # x valuex
    x <- j $ETmu
    plot_x <- unique(j$PlotID)
    
    # r-squared
    mod <- lm(y ~ x)
    tmp_rsq <- summary(mod)$r.squared
    tmp_p <- summary(mod)$coefficients[2, 4]
    
    # dissimilarity
    log_x_isna <- is.na(x)
    log_y_isna <- is.na(y)
    log_isna <- log_x_isna | log_y_isna
    if (any(log_isna)) {
      tmp_x_nona <- tmp_x[-which(log_isna)]
      tmp_y_nona <- tmp_y[-which(log_isna)]
    } else {
      tmp_x_nona <- x
      tmp_y_nona <- y
    }
    edist <- EuclideanDistance(tmp_x_nona, tmp_y_nona)
    
    data.frame(plot_y = plot_y, plot_x = plot_x, 
               rsq = tmp_rsq, p = tmp_p, edist = edist)
  })
  
  num_rsq <- sapply(ls_stats, "[[", 3)
  num_p <- sapply(ls_stats, "[[", 4)
  num_edist <- sapply(ls_stats, "[[", 5)
  
  list(num_rsq, num_p, num_edist)
}

mat_rsq <- sapply(ls_stats, "[[", 1)
mat_p <- sapply(ls_stats, "[[", 2)
mat_edist <- sapply(ls_stats, "[[", 3)

rownames(mat_edist) <- rownames(mat_p) <- rownames(mat_rsq) <- ch_plotid
colnames(mat_edist) <- colnames(mat_p) <- colnames(mat_rsq) <- ch_plotid

## reject low significant pixels
int_id_nonsignif <- which(mat_p > 0.001)
mat_rsq[int_id_nonsignif] <- NA
mat_edist[int_id_nonsignif] <- NA
mat_edist[mat_edist < 0.5] <- NA

## visualize
cols <- colorRampPalette(rev(brewer.pal(4, "Spectral")))
levelplot(mat_rsq, col.regions = cols(100), at = seq(0, 1, .05))
levelplot(mat_edist, col.regions = cols(100), at = seq(0, 1, .05))

