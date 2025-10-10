library(tidyverse)

#den lille tabel
calldf = as.data.frame(matrix(data=NA, nrow = 10, ncol = 10))
#
for(i in 1:10){
  for (j in 1:10){
    calldf[i,j]=i*abs(11-j)
  }
}
?abs

seq <- 11:2

10:1
###############3
#løb
####################


#vektorer, navne
Fornavne <- rep(c("Christian", "Viggo", "Casper", "Martin"), 3)

dato <- Sys.Date()
tidspunkt <- rep(c(dato-7, dato-14, dato-21), each=4)

resultater <- sample((20:40), 12)

løb <- data.frame(Fornavne=Fornavne, Tidspunkt=tidspunkt, Resultater=resultater)


######################
#stamdata
#####################

names <- c("Christian", "Viggo", "Egon", "Martin")
age <- sample(20:30, 4, replace=T)
road <- c("Tjørnevej", "Amagerbrogade", "Aaboulevarden", "Kastanjevænget")
zip <- c(8860, 2300, 2400, 9900)

adresser <- data.frame(Fornavne=names, Alder=age, Vej=road, `Postnr.`=zip)

################
#merge og gennemsnit
##############3

løbogstam <- left_join(løb, adresser)

avrundf <- løb %>% group_by(Fornavne) %>% 
  summarise(avtid=mean(resultater))

#####################3
#lister¨
#####################

#lav en liste over mulige hold med 1 til 4 deltagere

testliste=list()
testliste["names"]=list(c("Kurt", "Anthon"))
testliste["parts"]=list(c("Anthon"))
testliste["girls"]=list(c("Mona", "Ib", "Anthon"))
testliste["scores"]=list(1:200)

for(element in testliste) {
  print(element)
}

# loop igennem og gør noget ved hvert element
lapply(testliste, function(x) length(x))

names
# kombinationer af hold i en liste
df2=combn(names,2,simplify = F)

teamlist=list()
for(i in 1:length(names)) {
  #lav en kombination af i holdstørrelse  
  df2=combn(names,i,simplify = F)
  # put ind i listen
  teamlist[i]=list(df2)
}











#############
##loop igennem og gør noget ved hvert element
#lapply(testliste, function(x) length(x))

#kombination af hold i en liste
#df1=as.data.frame(combn(names, 1, simplify = F))
#df2=as.data.frame(t(as.data.frame((combn(names, 2, simplify = F)))))
#df3=as.data.frame(t(as.data.frame((combn(names, 3, simplify = F)))))

#teamlist=list()
#for(i in 1:length(names)) {
#  i=3
  #lav en kombination af i holdstørrelse
#  df2=as.data.frame(t(as.data.frame((combn(names, 2, simplify = F)))))
#put ind i listen
 # print(i)
  #teamlist[i]=list(df2)

}
  