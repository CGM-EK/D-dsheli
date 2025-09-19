FORV <- readxl::read_excel("FORV0025.xlsx")


##################################################
#1#
#Vælg denne for visning af 1. kvartal 2000 til 2. kvartal 2016
FORV0025 <- data.frame(FORV[1:198, ])

#2
#Vælg denne for visning af 1. kvartal 2000 til 2. kvartal 2025
FORV0025 <- data.frame(FORV)
####################################################
seq1 <- seq(1, nrow(FORV0025)-2, 3)
seq2 <- seq(2, nrow(FORV0025)-1, 3)
seq3 <- seq(3, nrow(FORV0025), 3)

fam_idag_etår <- FORV0025[ ,3]
DST1 <- (fam_idag_etår[seq1]+fam_idag_etår[seq2]+fam_idag_etår[seq3])/3

fam_etår_idag <- FORV0025[ ,4]
DST2 <- (fam_etår_idag[seq1]+fam_etår_idag[seq2]+fam_etår_idag[seq3])/3

dan_idag_etår <- FORV0025[ ,5]
DST3 <- (dan_idag_etår[seq1]+dan_idag_etår[seq2]+dan_idag_etår[seq3])/3

dan_etår_idag <- FORV0025[ ,6]
DST4 <- (dan_etår_idag[seq1]+dan_etår_idag[seq2]+dan_etår_idag[seq3])/3

forbrug_nu <- FORV0025[ ,7]
DST5 <- (forbrug_nu[seq1]+forbrug_nu[seq2]+forbrug_nu[seq3])/3

forbrug_12mnd <- FORV0025[ ,8]
DST6 <- (forbrug_12mnd[seq1]+forbrug_12mnd[seq2]+forbrug_12mnd[seq3])/3

C_t=0.1*DST1+0.2*DST3+0.17*DST5+0.53*DST6

plot(C_t)
lines(C_t)
