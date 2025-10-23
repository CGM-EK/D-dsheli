cols <- colnames(forbrugertillid)

for (i in 2:11) {
  # generate all combinations of columns of size i
  Comblist <- combn(cols, i, simplify = FALSE)
  
  # compute correlations for each combination
  cordf <- lapply(Comblist, function(vars) {
    combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  })
  
  # convert to a data frame with names
  cordf_df <- as.data.frame(cordf)
  
  #tilføjer navne til cordf
  colnames(cordf_df)[1:ncol(cordf_df)] = sapply(Comblist, paste, collapse = " + ")
  
  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("cordf", i), cordf_df)
}

totalcordf <- cbind(cordf2, cordf3, cordf4, cordf5, cordf6, cordf7, cordf8, cordf9, cordf10, cordf11)