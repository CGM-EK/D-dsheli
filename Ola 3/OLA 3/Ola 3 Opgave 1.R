#loader pakker til brug i opgaven
library(tidyverse)
library(ggplot2)
library(dkstat)

#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid1 <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid1 <- f.tillid1 %>% filter(TID >="2000-01-01")

f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#vi henter data fra Danmarks statistik
p.forbrugss <- dst_meta(table = "NKN1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
pforbrug_meta_filters <- list(
  TRANSAKT = "P.31 Privatforbrug",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)
p.forbrug1 <- dst_get_data(table = "NKN1", query = pforbrug_meta_filters, lang = "da")
p.forbrug <- p.forbrug1 %>% filter(TID >="1999-01-01")

f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerft1 <- f.tillid[c(kvartalseq1),3:ncol(f.tillid)]
kvartalerft2 <- f.tillid[c(kvartalseq2),3:ncol(f.tillid)]
kvartalerft3 <- f.tillid[c(kvartalseq3),3:ncol(f.tillid)]
forbrugertillid <- as.data.frame(c((kvartalerft1+kvartalerft2+kvartalerft3)/3))

#der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

###TESTER###
comb5 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb7 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)
comb9 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb10 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)

#Looper

Comblist=list()
for(i in 1:length(forbrugertillid)-1) {
  #lav en kombination af i holdstørrelse
  df2=combn(forbrugertillid,i,simplify = F)
  
  temp <- combn(
    x = as.numeric(forbrugertillid[i, ]),
    m = j,
    simplify = TRUE
  )
  
  # put ind i listen
  Comblist[i]=list(df2)
}

cordf <- lapply(forbrugertillid, function(x) cor.test(x, f.tillidsammen$pfv))

cols <- colnames(forbrugertillid)
Comblist1 <- combn(cols, 2, simplify = FALSE)  # pairs, change 2→i if needed

cordf <- lapply(Comblist1, function(vars) {
  combo_mean <- rowMeans(forbrugertillid[, vars, drop = FALSE])
  cor(combo_mean, f.tillidsammen$pfv)
})

### Pivot_Longer på CORDF ### Outputter en data frame med COR + rækkenummer combinationsnr.
cordf1 <- as.data.frame(cordf) 
cor_lang1 <- cordf1 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang1 = cor_lang1[-1]

cordf2 <- as.data.frame(cordf2) 
cor_lang2 <- cordf2 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang2 = cor_lang2[-1]

cordf3 <- as.data.frame(cordf3) 
cor_lang3 <- cordf3 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang3 = cor_lang3[-1]

cordf4 <- as.data.frame(cordf4)
cor_lang4 <- cordf4 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang4 = cor_lang4[-1]

cordf5 <- as.data.frame(cordf5)
cor_lang5 <- cordf5 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang5 = cor_lang5[-1]

cordf6 <- as.data.frame(cordf6)
cor_lang6 <- cordf6 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang6 = cor_lang6[-1]

cordf7 <- as.data.frame(cordf7)
cor_lang7 <- cordf7 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang7 = cor_lang7[-1]

cordf8 <- as.data.frame(cordf8)
cor_lang8 <- cordf8 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang8 = cor_lang8[-1]

cordf9 <- as.data.frame(cordf9)
cor_lang9 <- cordf9 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang9 = cor_lang9[-1]

cordf10 <- as.data.frame(cordf10)
cor_lang10 <- cordf10 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang10 = cor_lang10[-1]

cordf11 <- as.data.frame(cordf11)
cor_lang11 <- cordf11 %>% pivot_longer(cols = everything(),names_to = "corr", values_to = "Values")
cor_lang11 = cor_lang11[-1]

### Kontrol af Cor i listen ### BRUGES IKKE
førsteplads <- (forbrugertillid$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                        forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                        forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                        forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                        forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                        forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                        forbrugertillid$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/7

### LM TEST for top 5 Korrelationer ###
lmtest1 <- lm(f.tillidsammen$pfv ~ førsteplads)
summary(lmtest1)

andenplads <- (forbrugertillid$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                  forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                  forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                  forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                  forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                  forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/6


lmtest2 <- lm(f.tillidsammen$pfv ~ andenplads)
summary(lmtest2)

tredjeplads <- (forbrugertillid$F3.Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag+
                  forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                  forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                  forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                  forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                  forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                  forbrugertillid$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/7

lmtest3 <- lm(f.tillidsammen$pfv ~ tredjeplads)
summary(lmtest3)

fjerdeplads <- (forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                 forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                 forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                 forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                 forbrugertillid$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/6

lmtest4 <- lm(f.tillidsammen$pfv ~ fjerdeplads)
summary(lmtest4)

femteplads <- (forbrugertillid$F3.Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                 forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                 forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                 forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder)/6

lmtest5 <- lm(f.tillidsammen$pfv ~ femteplads)
summary(lmtest5)
