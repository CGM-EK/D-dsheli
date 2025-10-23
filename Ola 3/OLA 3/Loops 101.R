forbrugertillid101 <- forbrugertillid[1:101,]



Comblist101=list()
for(i in 1:length(forbrugertillid101)-1) {
  #lav en kombination af i holdstørrelse
  df2=combn(forbrugertillid101,i,simplify = F)
  
  temp <- combn(
    x = as.numeric(forbrugertillid101[i, ]),
    m = 1,
    simplify = TRUE
  )
  
  # put ind i listen
  Comblist101[i]=list(df2)
}

####################################


cols <- colnames(forbrugertillid101)
comb101=list()
for (j in 1:101){
for (i in 2:11) {
  # generate all combinations of columns of size i
  Comblist101 <- combn(cols, i, simplify = FALSE)
  
  # compute correlations for each combination
  cordf101 <- lapply(Comblist101, function(vars) {
    combo_mean101 <- rowMeans(forbrugertillid101[, vars, drop = FALSE])
    cor(combo_mean101, f.tillidsammen$pfv[1:j])
  })
  
  # convert to a data frame with names
  cordf_df101 <- data.frame(
    Combination101 = sapply(Comblist101, paste, collapse = " + "),
    Correlation101 = unlist(cordf101)
  )
  
  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("cordf101", i), cordf_df101)
  comb101[i]=list(cordf101)
  
  
  
  
}
}
