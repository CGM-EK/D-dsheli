#forward selection

#lineære modeller bliver kørt på alle variabler
lm.test.spg1 <- lm(dfalt$pfv~dfalt$fam.sit.bag)
lm.test.spg2 <- lm(dfalt$pfv~dfalt$fam.sit.frem)
lm.test.spg3 <- lm(dfalt$pfv~dfalt$dk.sit.bag)
lm.test.spg4 <- lm(dfalt$pfv~dfalt$dk.sit.frem)
lm.test.spg5 <- lm(dfalt$pfv~dfalt$an.str.fbg)

#p-værdi aflæses på hver variabel
summary(lm.test.spg1)
#p-værdi på 1.59e-09
summary(lm.test.spg2)
#p-værdi på 4.74e-05
summary(lm.test.spg3)
#p-værdi på 2.16e-12
summary(lm.test.spg4)
#p-værdi på 0.0597
summary(lm.test.spg5)
#p-værdi på 2.96e-08

#de to variabler med lavest p-værdi indsættes i en MLR
lm.test.fs1 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$fam.sit.bag)
summary(lm.test.fs1)

#Variablen med 3. lavest p-værdi indsættes i vores MLR
lm.test.fs2 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$fam.sit.bag+dfalt$an.str.fbg)
summary(lm.test.fs2)

#Da den 3. variabel ikke forbedrede modellen må vi konkludere at test 1 var optimal