#loader pakker til brug i opgaven
library(tidyverse)
library(ggplot2)
library(dkstat)
library(pls)
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

colnames(f.tillid)[2:14] <- c("FTI", "Spg1", "Spg2", "Spg3", "Spg4", "Spg8", "Spg5", "Spg6", "Spg7", "Spg9", "Spg10", "Spg11", "Spg12")

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
##########Sætter kolonne 8,9,10 og 12 i negativ####
f.tillid[,c(8,9,10,12)] <- f.tillid[,c(8,9,10,12)]*-1
#########################

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

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]

#### TESTER ####
comb5 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb7 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)
comb9 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb10 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)
#### TESTER ####

#Looper
#Er work in progress 23/10, da loopet skal opdateres####

Comblist=list()
for(i in 1:length(forbrugertillid)-1) {
  #lav en kombination af i holdstørrelse
  df2=combn(forbrugertillid,i,simplify = F)
  
  temp <- combn(
    x = as.numeric(forbrugertillid[i, ]),
    m = 1,
    simplify = TRUE
    
  )
  
  Comblist[i]=list(df2)
}

### Kontrol af Cor i listen ### BRUGES IKKE
førsteplads <- (forbrugertillid$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                  forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                  forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                  forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                  forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                  forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                  forbrugertillid$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener)/7

#### LM TEST for top 5 Korrelationer ####
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

#### LM TEST for top 5 Korrelationer ####

#### PCR.fit ####
install.packages("pls")
library(pls)

pcr.fit <- pcr(f.tillidsammen$pfv ~ forbrugertillid$F2.Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                 forbrugertillid$F3.Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F4.Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden+
                 forbrugertillid$F5.Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F6.Priser.i.dag..sammenlignet.med.for.et.år.siden+
                 forbrugertillid$F7.Priser.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F8.Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag+
                 forbrugertillid$F9.Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket+
                 forbrugertillid$F10.Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+
                 forbrugertillid$F12.Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder+
                 forbrugertillid$F13.Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener,
               validation = "CV", scale = T)
summary(pcr.fit)

loadings.pcr.fit <- pcr.fit$loadings

validationplot(pcr.fit, val.type = "MSEP")

w.indicators1 <- loadings.pcr.fit[1:11, 2]^2
sum(w.indicators1)

