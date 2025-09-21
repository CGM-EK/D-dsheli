install.packages("devtools")
install.packages("danstat")
library(danstat)
danstat?
danstat::get_data()
devtools::install_github("rOpenGov/dkstat")
1
library(dkstat)
dkbefolkning <- dst_meta(table = "FOLK1AM", lang = "da")
df1 <- as.data.frame(dkbefolkning$values$OMRÅDE)
dkbefolk_meta_filters <- list(
  OMRÅDE = "*",
  Tid = "2025M08"
)
befolkningsdata <- dst_get_data(table = "FOLK1AM", query = dkbefolk_meta_filters, lang = "da")
befolkningsdata$OMRÅDE <- gsub("[^A-Za-zÆØÅæøå]","",befolkningsdata$OMRÅDE)

names(befolkningsdata)[names(befolkningsdata)=="befolkningstal"] <- "BEFOLKNINGSTAL"
summary(befolkningsdata$BEFOLKNINGSTAL)

library(tidyverse)
?grep
befolkningsdatasub <- subset(befolkningsdata$OMRÅDE != grep("Region", befolkningsdata$OMRÅDE), TRUE)
?subset
befolkningsdatasub <- as.data.frame(befolkningsdata[-grep("Region", befolkningsdata$OMRÅDE),])
befolkningsdatasub <- as.data.frame(befolkningsdatasub[-grep("Helelandet", befolkningsdatasub$OMRÅDE),])

summary(befolkningsdatasub$BEFOLKNINGSTAL)

mybreaks=c(700000,60000, 10000, 3000, 750,0)
mylabs=c("landsby","lille by", "almindelig by", "større by", "storby")

befolkningsdatasub$bykat <- cut(befolkningsdatasub$BEFOLKNINGSTAL, labels = mylabs, breaks = mybreaks)
