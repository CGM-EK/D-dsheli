cols <- colnames(forbrugertillid)

for (i in 2:11) {
  # generate all combinations of columns of size i
  Comblist <- combn(cols, i, simplify = FALSE)

  # compute correlations for each combination
  cordf <- lapply(Comblist, function(vars) {
    combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
    cor(combo_mean, f.tillidsammen$pfv)
  })

  # convert to a data frame with names
  cordf_df <- data.frame(
    Combination = sapply(Comblist, paste, collapse = " + "),
    Correlation = unlist(cordf)
  )

  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("cordf", i), cordf_df)
}

