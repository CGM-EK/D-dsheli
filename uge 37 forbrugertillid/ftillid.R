forbrugertillidsindikator$year <- gsub("[^0-9]","",forbrugertillidsindikator$year)

kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

kvartalftillid1 <- Forbrugertillidsindikator_2000_2025$Forbrugertillidsindikatoren[kvartalseq1]
kvartalftillid2 <- Forbrugertillidsindikator_2000_2025$Forbrugertillidsindikatoren[kvartalseq2]
kvartalftillid3 <- Forbrugertillidsindikator_2000_2025$Forbrugertillidsindikatoren[kvartalseq3]

Forbrugertillidind <- as.data.frame(c((kvartalftillid1+kvartalftillid2+kvartalftillid3)/3))

forbrugertillidprivatf <- as.data.frame(Privatfrealvaekst[-1])
forbrugertillidprivatf$forbrugertillidsindeks <- Forbrugertillidind$`c((kvartalftillid1 + kvartalftillid2 + kvartalftillid3)/3)`

lm.test.find.privatf <- lm(forbrugertillidprivatf$`Privatfrealvaekst[-1]`~forbrugertillidprivatf$forbrugertillidsindeks)
summary(lm.test.find.privatf)

cor(forbrugertillidprivatf$`Privatfrealvaekst[-1]`,forbrugertillidprivatf$forbrugertillidsindeks)

Privatfrealvaekst <- c(0, diff(log(Privatforbrug_1999_2025$Privatforbrug),lag=4)*100)
view(Privatfrealvaekst)
library(tidyverse)
lm.test.ftillid.ftillid <- lm(Privatforbrug2021$privatforbrug_vaekst~
                                forbrugertillidprivatf$forbrugertillidsindeks+
                                fam.sit.bag.p.forbrug$fam.oek.sit.bag+
                                fam.sit.frem.p.forbrug$fam.sit.frem+
                                dk.sit.bag.p.forbrug$dk.sit.bag+
                                dk.sit.frem.p.forbrug$dk.sit.frem+
                                an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillid.ftillid)
Privatforbrug2021$privatforbrug_vaekst <- yessirvaekst[-1]

lm.test.5spg <- lm(forbrugertillidprivatf$`Privatfrealvaekst[-1]`~forbrugertillidprivatf$fam.sit.bag+
     forbrugertillidprivatf$fam.sit.frem+
     forbrugertillidprivatf$dk.sit.bag+
     forbrugertillidprivatf$dk.sit.frem+
     forbrugertillidprivatf$an.str.fbg)
summary(lm.test.5spg)

lm.test.opt<- lm(forbrugertillidprivatf$`Privatfrealvaekst[-1]`~
                   forbrugertillidprivatf$dk.sit.bag)
summary(lm.test.opt)

residual.test <- lm.test.5spg$residuals
forudsagt.test <- fitted.values(lm.test.5spg)
1.23072434-2.07595143

plot(forudsagt.test, residual.test)

lm.test.opt$residuals
fitted.values(lm.test.opt)
residualtest <- lm.test.opt$residuals
forudsagttest <- fitted.values(lm.test.opt)
-0.7094105-2.07595143


y <- forbrugertillidprivatf$`Privatfrealvaekst[-1]`

RSS_5spg <- sum((y-forudsagt.test)^2)
RSS_5spg
R2_5spg <- 1-(RSS_5spg/TSS_pforbrug)
R2_5spg

TSS_pforbrug <- sum((y-mean(y))^2)


RSS_opt <- sum((y-forudsagttest)^2)
RSS_opt
R2_opt <- 1-(RSS_opt/TSS_pforbrug)
R2_opt
