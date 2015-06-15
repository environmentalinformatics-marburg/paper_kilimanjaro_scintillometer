#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rowColFromCellC(NumericMatrix mn, int nCellId) {
  
  int nRows = mn.nrow(), nCols = mn.ncol();
  
  int n = 0;
  IntegerVector aiOut(2);
  
  for (int j = 0; j < nRows; j++) {
    for (int k = 0; k < nCols; k++) {
      n += 1;
      
      if (n == nCellId) {
        aiOut[1] = k + 1;
        break;
      }
    }
    
    if (n == nCellId) {
      aiOut[0] = j + 1;
      break;
    }
  }
  
  return aiOut;
}


// [[Rcpp::export]]
int cellFromRowColC(NumericMatrix mn, int nRow, int nCol) {
  
  nRow -= 1, nCol -= 1;
  
  int nRows = mn.nrow(), nCols = mn.ncol();
  
  int o = 0;
  int nOut = 0;
  
  for (int m = 0; m < nRows; m++) {
    for (int n = 0; n < nCols; n++) {
      o += 1;
      
      if (m == nRow & n == nCol) {
        nOut = o;
        break;
      }
      
    }
    
    if (nOut > 0) {
      break;
    }

  }
  
  return nOut;
}


/*      ls_adj <- lapply(ls_rowcol, function(l) {
        
        o <- 0
        
        for (m in 1:int_nrow) {
          for (n in 1:int_ncol) {
            o <- o + 1
            
            if (m == l[1] & n == l[2]) break
          }
          
          if (m == l[1] & n == l[2]) break
        }
        
        data.frame(row = l[1], col = l[2], id = o)

      })
*/
