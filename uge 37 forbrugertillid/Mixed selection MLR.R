#Mixed selection

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
lm.test.ms1 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$fam.sit.bag)
summary(lm.test.ms1)

#Variablen med 3. lavest p-værdi indsættes i vores MLR
lm.test.ms2 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$fam.sit.bag+dfalt$an.str.fbg)
summary(lm.test.ms2)
#Både variabelerne med 2. og 3. højest p-værdi har nu ikke nogen signifikans

#variabelen med 4. højest p-værdi indsættes
lm.test.ms3 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$fam.sit.frem)
summary(lm.test.ms3)
#variabelen med 4. højest p-værdi havde ingen signifikant

lm.test.ms4 <- lm(dfalt$pfv~dfalt$dk.sit.bag+dfalt$dk.sit.frem)
summary(lm.test.ms4)
#variabelen med lavest p-værdi havde ingen signifikans

#vi kan så ud fra mixed selection konkludere at den optimale MLR er
lm(dfalt$pfv~dfalt$dk.sit.bag)