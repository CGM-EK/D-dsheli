ftillidvaekst <- c(0, diff(log(forbrugertillidprivatf199$forbrugertillidsindeks),lag=4)*100)
view(ftillidvaekst)
kvartalftillid199 <- Forbrugertillidsind1999_2021$Forbrugertillidsindikatoren[kvartalseq1]
kvartalftillid299 <- Forbrugertillidsind1999_2021$Forbrugertillidsindikatoren[kvartalseq2]
kvartalftillid399 <- Forbrugertillidsind1999_2021$Forbrugertillidsindikatoren[kvartalseq3]

Forbrugertillidind199 <- as.data.frame(c((kvartalftillid199+kvartalftillid299+kvartalftillid399)/3))

forbrugertillidprivatf199 <- as.data.frame(Privatforbrug2021)
forbrugertillidprivatf199$forbrugertillidsindeks <- Forbrugertillidind199$`c((kvartalftillid199 + kvartalftillid299 + kvartalftillid399)/3)`

lm.test.ftillidee <- lm(Privatforbrug2021$privatforbrug_vaekst~
                                fam.sit.bag.p.forbrug$fam.oek.sit.bag+
                                fam.sit.frem.p.forbrug$fam.sit.frem+
                                dk.sit.bag.p.forbrug$dk.sit.bag+
                                dk.sit.frem.p.forbrug$dk.sit.frem+
                                an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillidee)

lm.test.ftillidupt <- lm(Privatforbrug2021$privatforbrug_vaekst~
                          dk.sit.bag.p.forbrug$dk.sit.bag+
                           an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillidupt)

lm.test.ftillideeupd1 <- lm(Privatforbrug2021$privatforbrug_vaekst~
                          fam.sit.bag.p.forbrug$fam.oek.sit.bag+
                          fam.sit.frem.p.forbrug$fam.sit.frem+
                          dk.sit.frem.p.forbrug$dk.sit.frem+
                          an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillideeupd1)

lm.test.ftillideeupd2 <- lm(Privatforbrug2021$privatforbrug_vaekst~
                              fam.sit.frem.p.forbrug$fam.sit.frem+
                              dk.sit.frem.p.forbrug$dk.sit.frem+
                              an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillideeupd2)

lm.test.ftillideeupd3 <- lm(Privatforbrug2021$privatforbrug_vaekst~
                              fam.sit.frem.p.forbrug$fam.sit.frem+
                              dk.sit.frem.p.forbrug$dk.sit.frem)
summary(lm.test.ftillideeupd3)

lm.test.ftillideeupdupd4 <- lm(Privatforbrug2021$privatforbrug_vaekst~
                          fam.sit.bag.p.forbrug$fam.oek.sit.bag+
                          fam.sit.frem.p.forbrug$fam.sit.frem+
                          dk.sit.bag.p.forbrug$dk.sit.bag+
                          an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillideeupdupd4)

lm.test.ftillideeupd5 <- lm(Privatforbrug2021$privatforbrug_vaekst~
                                 fam.sit.bag.p.forbrug$fam.oek.sit.bag+
                                 fam.sit.frem.p.forbrug$fam.sit.frem+
                                 an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillideeupd5)

lm.test.ftillideeupd6 <- lm(fam.sit.bag.p.forbrug$fam.oek.sit.bag~
                          fam.sit.frem.p.forbrug$fam.sit.frem+
                          dk.sit.bag.p.forbrug$dk.sit.bag+
                          dk.sit.frem.p.forbrug$dk.sit.frem+
                          an.str.fbg.p.forbrug$an.str.fbg)
summary(lm.test.ftillideeupd6)
