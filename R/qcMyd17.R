qcMyd17 <- function(x, len = 8, qc = TRUE, dd = TRUE, 
                    cs = TRUE, cs_exclude = "01", 
                    scf = FALSE, scf_exclude = c("010", "011", "100"), ...) {
  
  stopifnot(require(Rsenal))
  
  int_bin <- number2binary(x, len)
  int_bin <- rev(int_bin)
  
  int_qc <- int_bin[1]
  int_ss <- int_bin[2]
  int_dd <- int_bin[3]
  int_cs <- paste(int_bin[4:5], collapse = "")
  int_scf <- int_bin[6:8]
  
  if (qc)
    if (int_qc == 1) x <- NA
  
  if (dd)
    if (int_dd == 1) x <- NA
  
  if (cs)
    if (int_cs %in% cs_exclude) x <- NA
  
  if (scf)
    if (int_scf %in% scf_exclude) x <- NA
  
  return(x)
}
