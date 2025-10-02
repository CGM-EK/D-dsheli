#loader pakker til brug i opgaven
library(readxl)

f.tillid <- read_excel("R/R projekter/Forbrugertillidsindikator2000_2025 OLA2.xlsx")

p.forbrug <- read_excel("R/R projekter/D-dsheli-main/uge 37 forbrugertillid/Privatforbrug 1999-2025.xlsx", 
                        sheet = "Ark1")

#oprettelse af faktorvariabel der viser om det kvartalvise årlige vækst er steget eller faldet
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
Dfopg3 <- as.data.frame(year)

P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
Dfopg3$pfv <- P.forbrugvaekst[-1]
Dfopg3$YN <- as.factor(ifelse(Dfopg3$pfv >=0, "1", "0"))
table(Dfopg3$YN)
# no yes 
# 24 78 

############################################
#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#variabler defineres for DI og DST's forbrugertillidsindikatorer i f.tillid
f.tillid$sammenlagtDI <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                              f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                              f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)

f.tillid$sammenlagtDST <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                               f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                               f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)/5)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerDIft1 <- f.tillid$sammenlagtDI[kvartalseq1]
kvartalerDIft2 <- f.tillid$sammenlagtDI[kvartalseq2]
kvartalerDIft3 <- f.tillid$sammenlagtDI[kvartalseq3]
forbrugertillidDI <- as.data.frame(c((kvartalerDIft1+kvartalerDIft2+kvartalerDIft3)/3))

kvartalerDSTft1 <- f.tillid$sammenlagtDST[kvartalseq1]
kvartalerDSTft2 <- f.tillid$sammenlagtDST[kvartalseq2]
kvartalerDSTft3 <- f.tillid$sammenlagtDST[kvartalseq3]
forbrugertillidDST <- as.data.frame((kvartalerDSTft1+kvartalerDSTft2+kvartalerDSTft3)/3)

#der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

#Der oprettes en vektor for den årlige kvartalvise realvækst for privatforbruget, som indsættes i dataframes
P.forbrugvaekst <- c(0, diff(log(p.forbrug$value),lag=4)*100)
f.tillidsammen$pfv <- P.forbrugvaekst[-1]
f.tillidsammen$f.tillidDI <- forbrugertillidDI$`c((kvartalerDIft1 + kvartalerDIft2 + kvartalerDIft3)/3)`
f.tillidsammen$f.tillidDST <- forbrugertillidDST$`(kvartalerDSTft1 + kvartalerDSTft2 + kvartalerDSTft3)/3`

#lineære modeller for dst og di's forbrugertillidsindikatorer med fitted values og korrelation
lm.test.di <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDI)
summary(lm.test.di)
fitted.lm.test.di <- lm.test.di$fitted.values
cor(fitted.lm.test.di,f.tillidsammen$pfv)

lm.test.dst <- lm(f.tillidsammen$pfv~f.tillidsammen$f.tillidDST)
summary(lm.test.dst)
fitted.lm.test.dst <- lm.test.dst$fitted.values
cor(fitted.lm.test.dst,f.tillidsammen$pfv)

##
#indsætter de fittede værdier fra forbrugertillidsindikatorerne fra DST og DI i dataframen
Dfopg3$DI.fitted <- fitted.lm.test.di
Dfopg3$DST.fitted <- fitted.lm.test.dst


Dfopg3$YNDI <- as.factor(ifelse(Dfopg3$DI.fitted >=0, "1", "0"))
table(Dfopg3$YNDI)
#no  Yes 
#20  82 

Dfopg3$YNDST <- as.factor(ifelse(Dfopg3$DST.fitted >=0, "1", "0"))
table(Dfopg3$YNDST)
#no  Yes
#18  84 

glm.DIPFV <- glm(formula = Dfopg3$YN~Dfopg3$YNDI, family = "binomial")
summary(glm.DIPFV)

pred_probsDI <- predict(glm.DIPFV, type = "response")
threshold <- 0.5
pred_classDI <- ifelse(pred_probsDI > threshold,1, 0)
table(predicted =pred_classDI, actual = Dfopg3$YN)

glm.DSTPFV <- glm(formula = Dfopg3$YN~Dfopg3$YNDST, family = "binomial")
summary(glm.DSTPFV)

pred_probsDST <- predict(glm.DSTPFV, type = "response")
pred_classDST <- ifelse(pred_probsDST > threshold,1, 0)
table(predicted =pred_classDST, actual = Dfopg3$YN)

#########################

forbrugertillid_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
forbrugertillidspgs <- dst_get_data(table = "FORV1", query = forbrugertillid_meta_filters, lang = "da")
table(forbrugertillidspgs$INDIKATOR)
forbrugertillidspgs1 <- forbrugertillidspgs %>% filter(TID >="2000-01-01")

forbrugertillidspgs2 <- pivot_wider(
  data = forbrugertillidspgs1,
  names_from = INDIKATOR,
  values_from = value)

vurderingsdf <- as.data.frame(year)
vurderingsdf$fam.sit.bag <- fam.sit.ift.bag
vurderingsdf$fam.sit.frem <- fam.sit.frem
vurderingsdf$dk.sit.bag <- dk.sit.bag
vurderingsdf$dk.sit.frem <- dk.sit.frem
vurderingsdf$an.str.fbg.fd <- an.str.fbg.fd
vurderingsdf$an.str.fbg.n12 <- an.str.fbg.n12

vurderingsdf$fam.sit.bagYN <- as.factor(ifelse(vurderingsdf$fam.sit.bag >=0, "1", "0"))
vurderingsdf$fam.sit.fremYN <- as.factor(ifelse(vurderingsdf$fam.sit.frem >=0, "1", "0"))
vurderingsdf$dk.sit.bagYN <- as.factor(ifelse(vurderingsdf$dk.sit.bag >=0, "1", "0"))
vurderingsdf$dk.sit.fremYN <- as.factor(ifelse(vurderingsdf$dk.sit.frem >=0, "1", "0"))
vurderingsdf$an.str.fbg.fdYN <- as.factor(ifelse(vurderingsdf$an.str.fbg.fd >=0, "1", "0"))
vurderingsdf$an.str.fbg.n12YN <- as.factor(ifelse(vurderingsdf$an.str.fbg.n12 >=0, "1", "0"))

nydfopned <- forbrugertillidspgs %>% filter(TID>="2000-01-01")
nydfopned$valueopned <- as.factor(ifelse(nydfopned$value >=0, "yes", "no"))

ggplot(data = nydfopned, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity",position = "dodge")

df_counts <- nydfopned %>%
  group_by(INDIKATOR, valueopned) %>%
  summarise(n = n())

df_count_yes <- df_counts %>% filter(valueopned=="yes")
df_count_no <- df_counts %>% filter(valueopned=="no")
ggplot(data = df_count_yes, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity", position = "dodge")
ggplot(data = df_count_no, aes(x=INDIKATOR, y=n, fill = INDIKATOR))+
  geom_bar(stat = "identity", position = "dodge")

####
f.tillidspgkv <- as.data.frame(year)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerspg11 <- f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq1]
kvartalerspg12 <- f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq2]
kvartalerspg13 <- f.tillid$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq3]
f.tillidspgkv$spg1 <- c((kvartalerspg11+kvartalerspg12+kvartalerspg13)/3)

kvartalerspg21 <- f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq1]
kvartalerspg22 <- f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq2]
kvartalerspg23 <- f.tillid$`F3 Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq3]
f.tillidspgkv$spg2 <- c((kvartalerspg21+kvartalerspg22+kvartalerspg23)/3)

kvartalerspg31 <- f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq1]
kvartalerspg32 <- f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq2]
kvartalerspg33 <- f.tillid$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq3]
f.tillidspgkv$spg3 <- c((kvartalerspg31+kvartalerspg32+kvartalerspg33)/3)

kvartalerspg41 <- f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq1]
kvartalerspg42 <- f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq2]
kvartalerspg43 <- f.tillid$`F5 Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq3]
f.tillidspgkv$spg4 <- c((kvartalerspg41+kvartalerspg42+kvartalerspg43)/3)

kvartalerspg51 <- f.tillid$`F6 Priser i dag, sammenlignet med for et år siden`[kvartalseq1]
kvartalerspg52 <- f.tillid$`F6 Priser i dag, sammenlignet med for et år siden`[kvartalseq2]
kvartalerspg53 <- f.tillid$`F6 Priser i dag, sammenlignet med for et år siden`[kvartalseq3]
f.tillidspgkv$spg5 <- c((kvartalerspg51+kvartalerspg52+kvartalerspg53)/3)

kvartalerspg61 <- f.tillid$`F7 Priser om et år, sammenlignet med i dag`[kvartalseq1]
kvartalerspg62 <- f.tillid$`F7 Priser om et år, sammenlignet med i dag`[kvartalseq2]
kvartalerspg63 <- f.tillid$`F7 Priser om et år, sammenlignet med i dag`[kvartalseq3]
f.tillidspgkv$spg6 <- c((kvartalerspg61+kvartalerspg62+kvartalerspg63)/3)

kvartalerspg71 <- f.tillid$`F8 Arbejdsløsheden om et år, sammenlignet med i dag`[kvartalseq1]
kvartalerspg72 <- f.tillid$`F8 Arbejdsløsheden om et år, sammenlignet med i dag`[kvartalseq2]
kvartalerspg73 <- f.tillid$`F8 Arbejdsløsheden om et år, sammenlignet med i dag`[kvartalseq3]
f.tillidspgkv$spg7 <- c((kvartalerspg71+kvartalerspg72+kvartalerspg73)/3)

kvartalerspg81 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq1]
kvartalerspg82 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq2]
kvartalerspg83 <- f.tillid$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq3]
f.tillidspgkv$spg8 <- c((kvartalerspg81+kvartalerspg82+kvartalerspg83)/3)

kvartalerspg91 <- f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq1]
kvartalerDSTft92 <- f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq2]
kvartalerDSTft93 <- f.tillid$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq3]
f.tillidspgkv$spg9 <- c((kvartalerspg91+kvartalerDSTft92+kvartalerDSTft93)/3)

kvartalerspg111 <- f.tillid$`F11 Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[kvartalseq1]
kvartalerDSTft112 <- f.tillid$`F11 Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[kvartalseq2]
kvartalerDSTft113 <- f.tillid$`F11 Anser det som fornuftigt at spare op i den nuværende økonomiske situation`[kvartalseq3]
f.tillidspgkv$spg11 <- c((kvartalerspg111+kvartalerDSTft112+kvartalerDSTft113)/3)

kvartalerspg121 <- f.tillid$`F12 Regner med at kunne spare op i de kommende 12 måneder`[kvartalseq1]
kvartalerDSTft122 <- f.tillid$`F12 Regner med at kunne spare op i de kommende 12 måneder`[kvartalseq2]
kvartalerDSTft123 <- f.tillid$`F12 Regner med at kunne spare op i de kommende 12 måneder`[kvartalseq3]
f.tillidspgkv$spg12 <- c((kvartalerspg121+kvartalerDSTft122+kvartalerDSTft123)/3)


kvartalerspg131 <- f.tillid$`F13 Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[kvartalseq1]
kvartalerDSTft132 <- f.tillid$`F13 Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[kvartalseq2]
kvartalerDSTft133 <- f.tillid$`F13 Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener`[kvartalseq3]
f.tillidspgkv$spg13 <- c((kvartalerspg131+kvartalerDSTft132+kvartalerDSTft133)/3)

f.tillidspgkv$YN1 <- as.factor(ifelse(f.tillidspgkv$spg1 >=0, "1", "0"))
f.tillidspgkv$YN2 <- as.factor(ifelse(f.tillidspgkv$spg2 >=0, "1", "0"))
f.tillidspgkv$YN3 <- as.factor(ifelse(f.tillidspgkv$spg3 >=0, "1", "0"))
f.tillidspgkv$YN4 <- as.factor(ifelse(f.tillidspgkv$spg4 >=0, "1", "0"))
f.tillidspgkv$YN5 <- as.factor(ifelse(f.tillidspgkv$spg5 >=0, "1", "0"))
f.tillidspgkv$YN6 <- as.factor(ifelse(f.tillidspgkv$spg6 >=0, "1", "0"))
f.tillidspgkv$YN7 <- as.factor(ifelse(f.tillidspgkv$spg7 >=0, "1", "0"))
f.tillidspgkv$YN8 <- as.factor(ifelse(f.tillidspgkv$spg8 >=0, "1", "0"))
f.tillidspgkv$YN9 <- as.factor(ifelse(f.tillidspgkv$spg9 >=0, "1", "0"))
f.tillidspgkv$YN11 <- as.factor(ifelse(f.tillidspgkv$spg11 >=0, "1", "0"))
f.tillidspgkv$YN12 <- as.factor(ifelse(f.tillidspgkv$spg12 >=0, "1", "0"))
f.tillidspgkv$YN13 <- as.factor(ifelse(f.tillidspgkv$spg13 >=0, "1", "0"))
#
glm.spg1 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg1, family = "binomial")
summary(glm.spg1)

pred_probsspg1 <- predict(glm.spg1, type = "response")
threshold <- 0.5
pred_classspg1 <- ifelse(pred_probsspg1 > threshold,1, 0)
table(predicted =pred_classspg1, actual = Dfopg3$YN)
#
glm.spg2 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg2, family = "binomial")
summary(glm.spg2)

pred_probsspg2 <- predict(glm.spg2, type = "response")
threshold <- 0.5
pred_classspg2 <- ifelse(pred_probsspg2 > threshold,1, 0)
table(predicted =pred_classspg2, actual = Dfopg3$YN)
#
glm.spg3 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg3, family = "binomial")
summary(glm.spg3)

pred_probsspg3 <- predict(glm.spg3, type = "response")
threshold <- 0.5
pred_classspg3 <- ifelse(pred_probsspg3 > threshold,1, 0)
table(predicted =pred_classspg3, actual = Dfopg3$YN)
#
glm.spg4 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg4, family = "binomial")
summary(glm.spg4)

pred_probsspg4 <- predict(glm.spg4, type = "response")
threshold <- 0.5
pred_classspg4 <- ifelse(pred_probsspg4 > threshold,1, 0)
table(predicted =pred_classspg4, actual = Dfopg3$YN)
#
glm.spg5 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg5, family = "binomial")
summary(glm.spg5)

pred_probsspg5 <- predict(glm.spg1, type = "response")
threshold <- 0.5
pred_classspg5 <- ifelse(pred_probsspg5 > threshold,1, 0)
table(predicted =pred_classspg5, actual = Dfopg3$YN)

glm.spg6 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg6, family = "binomial")
summary(glm.spg6)

pred_probsspg6 <- predict(glm.spg6, type = "response")
threshold <- 0.5
pred_classspg6 <- ifelse(pred_probsspg6 > threshold,1, 0)
table(predicted =pred_classspg6, actual = Dfopg3$YN)

glm.spg7 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg7, family = "binomial")
summary(glm.spg7)

pred_probsspg7 <- predict(glm.spg7, type = "response")
threshold <- 0.5
pred_classspg7 <- ifelse(pred_probsspg7 > threshold,1, 0)
table(predicted =pred_classspg7, actual = Dfopg3$YN)

glm.spg8 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg8, family = "binomial")
summary(glm.spg8)

pred_probsspg8 <- predict(glm.spg8, type = "response")
threshold <- 0.5
pred_classspg8 <- ifelse(pred_probsspg8 > threshold,1, 0)
table(predicted =pred_classspg8, actual = Dfopg3$YN)

glm.spg9 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg9, family = "binomial")
summary(glm.spg9)

pred_probsspg9 <- predict(glm.spg9, type = "response")
threshold <- 0.5
pred_classspg9 <- ifelse(pred_probsspg9 > threshold,1, 0)
table(predicted =pred_classspg9, actual = Dfopg3$YN)

glm.spg11 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg11, family = "binomial")
summary(glm.spg11)

pred_probsspg11 <- predict(glm.spg11, type = "response")
threshold <- 0.5
pred_classspg11 <- ifelse(pred_probsspg11 > threshold,1, 0)
table(predicted =pred_classspg11, actual = Dfopg3$YN[1:101])

glm.spg12 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg12, family = "binomial")
summary(glm.spg12)

pred_probsspg12 <- predict(glm.spg12, type = "response")
threshold <- 0.5
pred_classspg12 <- ifelse(pred_probsspg12 > threshold,1, 0)
table(predicted =pred_classspg12, actual = Dfopg3$YN)


glm.spg13 <- glm(formula = Dfopg3$YN~f.tillidspgkv$spg13, family = "binomial")
summary(glm.spg13)

pred_probsspg13 <- predict(glm.spg13, type = "response")
threshold <- 0.5
pred_classspg13 <- ifelse(pred_probsspg13 > threshold,1, 0)
table(predicted =pred_classspg13, actual = Dfopg3$YN)

##########
f.tillidspgkv$voresfti1 <- f.tillidspgkv$spg8+f.tillidspgkv$spg9+f.tillidspgkv$spg3
f.tillidspgkv$YNfti1 <- as.factor(ifelse(f.tillidspgkv$voresfti1 >=0, "1", "0"))

glm.fti1 <- glm(formula = Dfopg3$YN~f.tillidspgkv$voresfti1, family = "binomial")
summary(glm.fti1)

pred_probsfti1 <- predict(glm.fti1, type = "response")
threshold <- 0.5
pred_classfti1 <- ifelse(pred_probsfti1 > threshold,1, 0)
table(predicted =pred_classfti1, actual = Dfopg3$YN)


f.tillidspgkv$voresfti2 <- f.tillidspgkv$spg8+f.tillidspgkv$spg9+f.tillidspgkv$spg3+f.tillidspgkv$spg5
f.tillidspgkv$YNfti2 <- as.factor(ifelse(f.tillidspgkv$voresfti2 >=0, "1", "0"))

glm.fti2 <- glm(formula = Dfopg3$YN~f.tillidspgkv$voresfti2, family = "binomial")
summary(glm.fti2)

pred_probsfti2 <- predict(glm.fti2, type = "response")
threshold <- 0.5
pred_classfti2 <- ifelse(pred_probsfti2 > threshold,1, 0)
table(predicted =pred_classfti2, actual = Dfopg3$YN)

