library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

api_key <- "SG_APIM_EZQHF5FB2Z4JCZY0M68FT9W22VEDWWGPZFF557JPVY61NFM4SHJG"
sep <- c(3400, 8900, 4800, 2300, 5700)

for(i in 1:5){

resp <- GET(
  paste0("https://api.sallinggroup.com/v1/food-waste?zip=",sep[i]),
  add_headers(Authorization = paste("Bearer", api_key))
)

data_raw <- content(resp, as = "text", encoding = "UTF-8")
data_json <- fromJSON(data_raw, flatten = TRUE)

assign(paste0("df", sep[i]), as.data.frame(data_json))

}
############

sampledf <- df4800[[1]][[1]]
#sampledf <- sampledf[,c(2,4,6,7,8,9,10,11,12,15)]

#timer som tilbuddene har været oppe####
unix_start <- as.numeric(as.POSIXct(sampledf$offer.startTime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
unix_end <- as.numeric(as.POSIXct(sampledf$offer.endTime, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
unix_total <- (unix_end-unix_start)/3600
#####

sampledf <- cbind(sampledf, timer_tilbud_har_været_oppe=unix_total)



