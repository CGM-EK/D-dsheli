stabil101 = as.data.frame(matrix(data=NA,  nrow = 1, ncol = ncol(totalcordf)))
for (i in 1:ncol(totalcordf)){
  lm.spgcomb <- lm(f.tillidsammen$pfv[10:102] ~ totalcordf[10:102,i])
  
  R2 <- summary(lm.spgcomb)$r.squared
  stabil101[,i] <- R2
}

colnames(stabil101) <- colnames(totalcordf)

stabil101flip <- t(stabil101)


totalcordfliste <- as.list(totalcordf)

indikatornr <- c(1:12)
score <- c(16,9,16,17,12,2,2,2,1,9,3,1)

indikatorscore <- as.data.frame(indikatornr)
indikatorscore$score <- score

ggplot(data = indikatorscore, aes(y = indikatorscore$score, x = indikatorscore$indikatornr))+
  geom_bar(stat = "identity", fill = "darkolivegreen4", color = "black")+
  scale_x_continuous(breaks = seq(0,12, by = 1))+
  labs(
    title = "Top 12 indikatorers forekomster i Top 5 i undersøgelse af stabilitet",
    x = "Indikatornummer",
    y = "Antal forekomster"
  )+
  theme_minimal()
  