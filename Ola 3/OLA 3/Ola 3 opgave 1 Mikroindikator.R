Mikrospg <- as.data.frame(forbrugertillid[c(1,2,9,11,12)])

Mikrocordf <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols <- colnames(Mikrospg)
MikroComblist2 <- combn(Mikrocols, 2, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf <- lapply(MikroComblist2, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

Mikrocordf3 <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols3 <- colnames(Mikrospg)
MikroComblist1 <- combn(Mikrocols, 3, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf3 <- lapply(MikroComblist1, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

Mikrocordf4 <- lapply(Mikrospg, function(x) cor.test(x, f.tillidsammen$pfv))

Mikrocols3 <- colnames(Mikrospg)
MikroComblist1 <- combn(Mikrocols, 4, simplify = FALSE)  # pairs, change 2→i if needed

Mikrocordf4 <- lapply(MikroComblist1, function(vars) {
  combo_mean <- rowMeans(Mikrospg[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})


### MIKROCORLANG ###

Mikrocordf <- as.data.frame(Mikrocordf)
Mikrocor_lang1 <- Mikrocordf %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang1 = Mikrocor_lang1[-1]

Mikrocordf3 <- as.data.frame(Mikrocordf3)
Mikrocor_lang3 <- Mikrocordf3 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang3 = Mikrocor_lang3[-1]

Mikrocordf4 <- as.data.frame(Mikrocordf4)
Mikrocor_lang4 <- Mikrocordf4 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
Mikrocor_lang4 = Mikrocor_lang4[-1]

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
Mikrolmtest1 <- lm(f.tillidsammen$pfv ~ MikroFørsteplads)
summary(Mikrolmtest1)

MikroFørsteplads <- (Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                      Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/2

Mikrolmtest2 <- lm(f.tillidsammen$pfv ~ MikroAndenplads)
summary(Mikrolmtest2)

MikroAndenplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                     Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/3

Mikrolmtest3 <- lm(f.tillidsammen$pfv ~ MikroTredjeplads)
summary(Mikrolmtest3)

MikroTredjeplads <- (Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/2

Mikrolmtest4 <- lm(f.tillidsammen$pfv ~ MikroFjerdeplads)
summary(Mikrolmtest4)

MikroFjerdeplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                       Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                       Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/4

Mikrolmtest5 <- lm(f.tillidsammen$pfv ~ MikroFemteplads)
summary(Mikrolmtest5)

MikroFemteplads <- (Mikrospg$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                       Mikrospg$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                       Mikrospg$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/3


