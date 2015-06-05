ch_dir_trees <- "../../phd/scintillometer/data/SP4_tree_inventory"
ch_fls_trees <- "10640.txt"
ch_fls_trees <- paste(ch_dir_trees, ch_fls_trees, sep = "/")

df_trees <- read.table(ch_fls_trees, header = TRUE, sep = "\t", 
                       stringsAsFactors = FALSE, dec = ",", 
                       na.strings = "-999999")

df_trees <- df_trees[, c("PLOTID", "SPEC", "HEIG", "CRO_A")]
df_trees_sub <- subset(df_trees, PLOTID %in% c("cof2", "cof3"))

ls_trees_sub <- split(df_trees_sub, f = df_trees_sub$PLOTID)
ls_trees_summary <- lapply(ls_trees_sub, function(i) {
  data.frame(PLOTID = i$PLOTID[1], 
             HEIG = mean(i$HEIG, na.rm = TRUE), 
             CRO_A = mean(i$CRO_A, na.rm = TRUE), 
             SPEC = modeFactor(i$SPEC), 
             COUNT = sum(i$SPEC == modeFactor(i$SPEC)), 
             COUNT_ELSE = sum(i$SPEC != modeFactor(i$SPEC)))
})
df_trees_summary <- do.call("rbind", ls_trees_summary)
