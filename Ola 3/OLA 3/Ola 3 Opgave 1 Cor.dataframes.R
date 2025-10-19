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


########################
cordf2 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols2 <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 2, simplify = FALSE)  # pairs, change 2→i if needed

cordf2 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf3 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 3, simplify = FALSE)  # pairs, change 2→i if needed

cordf3 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf4 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 4, simplify = FALSE)  # pairs, change 2→i if needed

cordf4 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf5 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 5, simplify = FALSE)  # pairs, change 2→i if needed

cordf5 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})


cordf6 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 6, simplify = FALSE)  # pairs, change 2→i if needed

cordf6 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf7 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 7, simplify = FALSE)  # pairs, change 2→i if needed

cordf7 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf8 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 8, simplify = FALSE)  # pairs, change 2→i if needed

cordf8 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf9 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 9, simplify = FALSE)  # pairs, change 2→i if needed

cordf9 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf10 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 10, simplify = FALSE)  # pairs, change 2→i if needed

cordf10 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

cordf11 <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 11, simplify = FALSE)  # pairs, change 2→i if needed

cordf11 <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})


cordftest <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 7, simplify = FALSE)  # pairs, change 2→i if needed

cordftest <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor.test(combo_mean, f.tillidsammen$pfv)
})
