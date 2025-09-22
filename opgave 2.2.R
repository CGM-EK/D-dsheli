f.tillid <- Forbrugertillidsindikator2000_2025_OLA2

p.forbrug <- Privatforbrug_1999_2025

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

f.tillid$sammenlagtDI <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                           f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                             f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                             f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4)
f.tillid$sammenlagtDST <- c((f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                               f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                               f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`+
                               f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)/5)

#kvartalsekvenser anvendes på forbrugertillidsindikatoren og dens underspørgsmpk og der oprettes vektorer

kvartalerDIft1 <- f.tillid$sammenlagtDI[kvartalseq1]
kvartalerDIft2 <- f.tillid$sammenlagtDI[kvartalseq2]
kvartalerDIft3 <- f.tillid$sammenlagtDI[kvartalseq3]
forbrugertillidDI <- c((kvartalerDIft1+kvartalerDIft2+kvartalerDIft3)/3)

kvartalerDSTft1 <- f.tillid$sammenlagtDST[kvartalseq1]
kvartalerDSTft2 <- f.tillid$sammenlagtDST[kvartalseq2]
kvartalerDSTft3 <- f.tillid$sammenlagtDST[kvartalseq3]
forbrugertillidDST <- c((kvartalerDSTft1+kvartalerDSTft2+kvartalerDSTft3)/3)

kvartalftillid1 <- f.tillid$Forbrugertillidsindikatoren[kvartalseq1]
kvartalftillid2 <- f.tillid$Forbrugertillidsindikatoren[kvartalseq2]
kvartalftillid3 <- f.tillid$Forbrugertillidsindikatoren[kvartalseq3]
forbrugertillid <- c((kvartalftillid1+kvartalftillid2+kvartalftillid3)/3)

kvartalfambag1 <- f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq1]
kvartalfambag2 <- f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq2]
kvartalfambag3 <- f.tillid$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq3]
fam.sit.ift.bag <- c((kvartalfambag1+kvartalfambag2+kvartalfambag3)/3)

kvartalfamfrem1 <- f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq1]
kvartalfamfrem2 <- f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq2]
kvartalfamfrem3 <- f.tillid$`Familiens økonomiske  situation om et år, sammenlignet med i dag`[kvartalseq3]
fam.sit.frem <- c((kvartalfamfrem1+kvartalfamfrem2+kvartalfamfrem3)/3)

kvartaldkbag1 <- f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq1]
kvartaldkbag2 <- f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq2]
kvartaldkbag3 <- f.tillid$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`[kvartalseq3]
dk.sit.bag <- c((kvartaldkbag1+kvartaldkbag2+kvartaldkbag3)/3)

kvartaldkfrem1 <- f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq1]
kvartaldkfrem2 <- f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq2]
kvartaldkfrem3 <- f.tillid$`Danmarks økonomiske situation om et år, sammenlignet med i dag`[kvartalseq3]
dk.sit.frem <- c((kvartaldkfrem1+kvartaldkfrem2+kvartaldkfrem3)/3)

kvartalstrfbg1 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq1]
kvartalstrfbg2 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq2]
kvartalstrfbg3 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq3]
an.str.fbg.fd <- c((kvartalstrfbg1+kvartalstrfbg2+kvartalstrfbg3)/3)

kvartalstrfbgn1 <- f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq1]
kvartalstrfbgn2 <- f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq2]
kvartalstrfbgn3 <- f.tillid$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`[kvartalseq3]
an.str.fbg.n12 <- c((kvartalstrfbgn1+kvartalstrfbgn2+kvartalstrfbgn3)/3)

#der oprettes en dataframe med første vektor som år
year <- c(rep(2000:2024, each = 4), rep(2025, 2))
dfDI <- as.data.frame(year)
dfDST <- as.data.frame(year)

#Der oprettes en vektor for årlig realvækst for privatforbruget, som indsættes i dataframet
P.forbrugvaekst <- c(0, diff(log(p.forbrug$Privatforbrug),lag=4)*100)
dfDI$pfv <- P.forbrugvaekst[-1]
dfDST$pfv <- P.forbrugvaekst[-1]

#DI's fti
dfDI$fam.sit.ift.bag <- fam.sit.ift.bag
dfDI$dk.sit.bag  <- dk.sit.bag
dfDI$an.str.fbg.fd <- an.str.fbg.fd
dfDI$an.str.fbg.n12 <- an.str.fbg.n12

#DST's fti
dfDST$fam.sit.ift.bag <- fam.sit.ift.bag
dfDST$fam.sit.frem <- fam.sit.frem
dfDST$dk.sit.bag <- dk.sit.bag
dfDST$dk.sit.frem <- dk.sit.frem
dfDST$an.str.fbg.fd <- an.str.fbg.fd

lm.test.di <- lm(dfDI$pfv~forbrugertillidDI)
summary(lm.test.di)
fitted.lm.test.di <- lm.test.di$fitted.values
cor(fitted.lm.test.di,dfDI$pfv)


lm.test.dst <- lm(dfDST$pfv~forbrugertillidDST)
summary(lm.test.dst)
fitted.lm.test.dst <- lm.test.dst$fitted.values
cor(fitted.lm.test.dst,dfDI$pfv)
fitted.lm.test.di
plot(lm.test.di)
plot(dfDI$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
     axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))
par(mfrow=c(1,2))
barplot(fitted.lm.test.di,dfDI$year, ylim = c(-25,25))
dfDI$year <- as.numeric(dfDI$year)

plot(dfDI2016$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
axis(side = 1, at = seq(1, 66, by = 4), labels = paste("", 0:16))

dfDI2016 <- as.data.frame(dfDI[1:66,])

plot(fitted.lm.test.di, type = "l")
plot(fitted.lm.test.di, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="DI's forbrugertillidsindikator")
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))


DI3k <- c(forbrugertillid_3_kvartal$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
           forbrugertillid_3_kvartal$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
           forbrugertillid_3_kvartal$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
           forbrugertillid_3_kvartal$`Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`)/4
DI3ksam <- sum(DI3k)/3

DST3k <- c(forbrugertillid_3_kvartal$`Familiens økonomiske situation i dag, sammenlignet med for et år siden`+
                      forbrugertillid_3_kvartal$`Danmarks økonomiske situation i dag, sammenlignet med for et år siden`+
                      forbrugertillid_3_kvartal$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`+
                      forbrugertillid_3_kvartal$`Familiens økonomiske  situation om et år, sammenlignet med i dag`+
                        forbrugertillid_3_kvartal$`Danmarks økonomiske situation om et år, sammenlignet med i dag`)/5
DST3ksam <- sum(DST3k)/3

DI3kpfv <- 2.09628+0.20139*DI3ksam

DST3ksam <- 1.19684+0.18502*DST3ksam
