madjacent <- function(x, cells, directions = 4, sorted = TRUE, as_list = TRUE) {

  ## load 'Rcpp' and required C++ functions
  stopifnot(require(Rcpp))
  sourceCpp("src/SlsCppFun.cpp")
  
  ## matrix dimensions
  int_nrow <- dim(x)[1]
  int_ncol <- dim(x)[2]
  
  ## loop over cells
  ls_adj <- lapply(cells, function(i) {
    
    int_rowcol <- rowColFromCellC(x, i)
    int_row <- int_rowcol[1]
    int_col <- int_rowcol[2]
    
    if (directions == 4) {
      
      ls_rowcol <- list(c(int_row-1, int_col), c(int_row+1, int_col), 
                        c(int_row, int_col-1), c(int_row, int_col+1))
      
    } else if (directions == 8) {
      
      ls_rowcol <- list(c(int_row-1, int_col-1), c(int_row-1, int_col), c(int_row-1, int_col+1), 
                        c(int_row, int_col-1), c(int_row, int_col+1), 
                        c(int_row+1, int_col-1), c(int_row+1, int_col), c(int_row+1, int_col+1))
      
    }
    
    int_xlim <- sapply(ls_rowcol, "[[", 1)
    int_ylim <- sapply(ls_rowcol, "[[", 2)
    int_excd <- which(int_xlim > int_nrow | int_ylim > int_ncol |
                        int_xlim < 1 | int_ylim < 1)
    
    if (length(int_excd) > 0)
      ls_rowcol <- ls_rowcol[-int_excd]
    
    int_adj <- sapply(ls_rowcol, function(l) {
      int_id <- cellFromRowColC(x, l[1], l[2])
    })
    
    return(int_adj)
  })

  if (!as_list) {
    int_adj <- do.call("c", ls_adj)
    int_adj <- unique(int_adj)
    
    if (sorted) 
      int_adj <- sort(int_adj)
    
    return(int_adj)
    
  } else {
  
    return(ls_adj)
  }
}