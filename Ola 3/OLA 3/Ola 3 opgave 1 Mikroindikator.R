Mikrospg <- as.data.frame(forbrugertillid[c(1,2,9,11,12)])
#### Nyt loop ####
Mikrocols <- colnames(Mikrospg)

for (i in 2:4) {
  # generate all combinations of columns of size i
  MikroComblist <- combn(Mikrocols, i, simplify = FALSE)
  
  # compute correlations for each combination
  Mikrocordf <- lapply(MikroComblist, function(vars) {
    combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
    cor(combo_mean, f.tillidsammen$pfv)
  })
  
  # convert to a data frame with names
  Mikrocordf <- data.frame(
    Combination = sapply(MikroComblist, paste, collapse = " + "),
    Correlation = unlist(Mikrocordf)
  )
  
  # dynamically assign it as cordf2, cordf3, etc.
  assign(paste0("Mikrocordf", i), Mikrocordf)
}

Mikrocordf2 <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols2 <- colnames(Mikrospg)
MikroComblist2 <- combn(Mikrocols2, 2, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf2 <- lapply(MikroComblist2, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

Mikrocordf3 <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols3 <- colnames(Mikrospg)
MikroComblist+ <- combn(Mikrocols, 3, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf3 <- lapply(MikroComblist1, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

Mikrocordf4 <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols4 <- colnames(Mikrospg)
MikroComblist1 <- combn(Mikrocols, 4, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf4 <- lapply(MikroComblist1, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})
#### Nyt loop ####

#### MIKROCORLANG ####

Mikrocordf2 <- as.data.frame(Mikrocordf2)
Mikrocor_lang1 <- Mikrocordf %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang1 = Mikrocor_lang1[-1]

Mikrocordf3 <- as.data.frame(Mikrocordf3)
Mikrocor_lang3 <- Mikrocordf3 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang3 = Mikrocor_lang3[-1]

Mikrocordf4 <- as.data.frame(Mikrocordf4)
Mikrocor_lang4 <- Mikrocordf4 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang4 = Mikrocor_lang4[-1]

for (i in 2:4) {
  df_nameMikro <- get(paste0("Mikrocordf", i))
  
  Mikrocor_lang <- as.data.frame(df_nameMikro[-1]) %>% 
    pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
  Mikrocor_lang = Mikrocor_lang[-1]
  
  assign(paste0("Mikrocor_lang", i), Mikrocor_lang)
}
#### MIKROCORLANG ####
### MIKROCOMBLIST ###

MikroComblist=list()
for(i in 1:length(Mikrospg)-1) {
  #lav en kombination af i holdstørrelse
  df3=combn(Mikrospg,i,simplify = F)
  
  temp <- combn(
    x = as.numeric(Mikrospg[i, ]),
    m = 1,
    simplify = TRUE
  )
  
  # put ind i listen
  MikroComblist[i]=list(df3)
}

### MIRKOLMTEST ###
MikroFørsteplads <- (Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/2

Mikrolmtest1 <- lm(f.tillidsammen$pfv ~ MikroFørsteplads)
summary(Mikrolmtest1)

MikroAndenplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                      Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                      Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/3

Mikrolmtest2 <- lm(f.tillidsammen$pfv ~ MikroAndenplads)
summary(Mikrolmtest2)

MikroTredjeplads <- (Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/2

Mikrolmtest3 <- lm(f.tillidsammen$pfv ~ MikroTredjeplads)
summary(Mikrolmtest3)

MikroFjerdeplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                       Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                       Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/4

Mikrolmtest4 <- lm(f.tillidsammen$pfv ~ MikroFjerdeplads)
summary(Mikrolmtest4)

MikroFemteplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                      Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                      Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/3


Mikrolmtest5 <- lm(f.tillidsammen$pfv ~ MikroFemteplads)
summary(Mikrolmtest5)

