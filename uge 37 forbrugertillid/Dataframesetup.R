#loader tidyverse
library(tidyverse)
library(readxl)

#dataset for privatforbrug indlæses
P.forbrug <- read_excel("~/R projekter/uge 37 forbrugertillid/Privatforbrug 1999-2025.xlsx", sheet = "Ark1")

#dataset for forbrugertillidsindikator indlæses
f.tillid <- read_excel("~/R projekter/uge 37 forbrugertillid/Forbrugertillidsindikator 2000-2025.xlsx", sheet = "Ark1")

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#kvartalsekvenser anvendes på forbrugertillidsindikatoren og dens underspørgsmpk og der oprettes vektorer
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
an.str.fbg <- c((kvartalstrfbg1+kvartalstrfbg2+kvartalstrfbg3)/3)

#der oprettes en dataframe med første vektor som år
year <- c(rep(2000:2024, each = 4), rep(2025, 2))
dfalt <- as.data.frame(year)

#Der oprettes en vektor for årlig realvækst for privatforbruget, som indsættes i dataframet
P.forbrugvaekst <- c(0, diff(log(P.forbrug$Privatforbrug),lag=4)*100)
dfalt$pfv <- P.forbrugvaekst[-1]

#forbrugertillidsindikatoren samt underspørgsmål indsættes så i det nye dataframe
dfalt$f.tillid <- forbrugertillid
dfalt$fam.sit.bag <- fam.sit.ift.bag
dfalt$fam.sit.frem <- fam.sit.frem
dfalt$dk.sit.bag <- dk.sit.bag
dfalt$dk.sit.frem <- dk.sit.frem
dfalt$an.str.fbg <- an.str.fbg

