library(tidyverse)
library(ggplot2)
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
forbrugertillidDI <- as.data.frame(c((kvartalerDIft1+kvartalerDIft2+kvartalerDIft3)/3))

kvartalerDSTft1 <- f.tillid$sammenlagtDST[kvartalseq1]
kvartalerDSTft2 <- f.tillid$sammenlagtDST[kvartalseq2]
kvartalerDSTft3 <- f.tillid$sammenlagtDST[kvartalseq3]
forbrugertillidDST <- c((kvartalerDSTft1+kvartalerDSTft2+kvartalerDSTft3)/3)

#der oprettes en dataframe med første vektor som år
year <- c(rep(2000:2024, each = 4), rep(2025, 2))
dfDI <- as.data.frame(year)
dfDST <- as.data.frame(year)

#Der oprettes en vektor for årlig realvækst for privatforbruget, som indsættes i dataframet
P.forbrugvaekst <- c(0, diff(log(p.forbrug$Privatforbrug),lag=4)*100)
dfDI$pfv <- P.forbrugvaekst[-1]
dfDST$pfv <- P.forbrugvaekst[-1]

#lineære modeller for dst og di's forbrugertillidsindikatorer
lm.test.di <- lm(dfDI$pfv~forbrugertillidDI)
summary(lm.test.di)
fitted.lm.test.di <- lm.test.di$fitted.values
cor(fitted.lm.test.di,dfDI$pfv)

lm.test.dst <- lm(dfDST$pfv~forbrugertillidDST)
summary(lm.test.dst)
fitted.lm.test.dst <- lm.test.dst$fitted.values
cor(fitted.lm.test.dst,dfDI$pfv)

#plots for at tjekke data
plot(dfDI$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))
par(mfrow=c(1,2))
barplot(fitted.lm.test.di,dfDI$year, ylim = c(-25,25))
dfDI$year <- as.numeric(dfDI$year)

dfDI2016 <- as.data.frame(dfDI[1:66,])
plot(dfDI2016$pfv, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="Årlig realvækst pr. kvartal i privat forbruget")
axis(side = 1, at = seq(1, 66, by = 4), labels = paste("", 0:16))

plot(fitted.lm.test.di, type = "l")
plot(fitted.lm.test.di, ylim = c(-8,8), type = "l", xaxt = "n", xlab = "year", ylab ="DI's forbrugertillidsindikator")
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))

plot(forbrugertillidDI$modeltal, ylim = c(-25,25), type = "l", xaxt = "n", xlab = "year", ylab = "DI's forbrugertillidsindikator")
axis(side = 1, at = seq(1, 106, by = 4), labels = paste("", 0:26))
axis(side = 2, at = seq(-25, 25, by = 8.5), labels = paste("", 0:25))

indikatorplus <- sum(f.tillid$sammenlagtDI)/nrow(f.tillid)

vplus <- c(rep(indikatorplus,102))

forbrugertillidDI$modeltal <- forbrugertillidDI$`c((kvartalerDIft1 + kvartalerDIft2 + kvartalerDIft3)/3)`+(vplus*-1)

#2016 ftillid DI
ftillid2016DI <- data.frame(forbrugertillidDI[1:66,1])
indikatorplus2016 <- sum(ftillid2016DI$forbrugertillidDI.1.66..1.)/nrow(ftillid2016DI)
vplus2016 <- c(rep(indikatorplus2016,66))
ftillid2016DI$modeltal <- (ftillid2016DI$forbrugertillidDI.1.66..1.)+(vplus2016*-1)
plot(ftillid2016DI$modeltal, ylim = c(-25,25), type = "l", xaxt = "n", xlab = "year", ylab = "DI's forbrugertillidsindikator")
axis(side = 1, at = seq(1, 66, by = 4), labels = paste("", 0:16))
names(ftillid2016DI)[1] <- "ftillidindDI"
?names
#plot for 2016
ftillid2016DI$pfv <- dfDI2016$pfv
kvartaler <- seq.Date(from = as.Date("2000-01-01"),
                      to = as.Date("2016-06-30"),
                      by = "quarter")
ftillid2016DI$kvartaler <- kvartaler
ftillid2016DI$year <- year16
ggplot(data = ftillid2016DI, aes(x=kvartaler))+
       geom_col(aes(y=pfv*3.125), fill = "steelblue")+
  geom_line(aes(y=modeltal), size = 0.7)+
  scale_y_continuous(
    name = "Nettotal",
    limits = c(-25,25),
    sec.axis = sec_axis(~./3.125, name ="PCT")
  )+ theme_minimal()+ labs(title = "DI's forbrugertillidsindikator følger i højere grad privatforbruget")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_x_date(breaks = ftillid2016DI$kvartaler[seq(1, length(ftillid2016DI$kvartaler), by = 4)],
                 labels = format(ftillid2016DI$kvartaler[seq(1, length(ftillid2016DI$kvartaler), by = 4)], "%Y"))

#forudsigelser for privatforbrug k3 fra DI og DST's Forbrugertillidsindikator
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