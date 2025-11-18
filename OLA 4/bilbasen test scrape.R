#scrape make, model and price

library(rvest)
library(httr)
library(stringr)
library(dplyr)

makemodels <- c()
prices <- c()
facts <- c()
locations <- c()


url <- "https://www.bilbasen.dk/brugt/autocamper?fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&sellertypes=dealer"
res <- GET(url, add_headers("User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36"))
page <- read_html(res)
cars <- page %>% html_nodes("article.Listing_listing__XwaYe")


########################################

for(page_num in 2:6){
  url <- paste0("https://www.bilbasen.dk/brugt/autocamper?includeengroscvr=true&includeleasing=false&sellertypes=dealer&page=", page_num)
  page <- read_html(url)
  cars <- page %>% html_nodes("article.Listing_listing__XwaYe")
  
  for(car in cars){
    makemodel <- car %>% 
      html_node("h3.font-bold") %>% 
      html_text()
    makemodels <- c(makemodels, makemodel)
    
    price <- car %>% 
      html_node("h3.font-bold.color-primary") %>%
      html_text()
    prices <- c(prices, price)
    
    fact <- car %>% 
      html_node("li.ListingDetails_listItem___omDg") %>% 
      html_text()
    facts <- c(facts, fact)
    
  }
}


df <- data.frame(makemodels=makemodels, prices=prices, facts=facts)



############


for(car in cars){
  makemodel <- car %>% 
    html_node("h3.font-bold") %>% 
    html_text()
  makemodels <- c(makemodels, makemodel)
  
  price <- car %>% 
    html_node("h3.font-bold.color-primary") %>%
    html_text()
  prices <- c(prices, price)
  
  fact <- car %>% 
    html_node("li.ListingDetails_listItem___omDg") %>% 
    html_text()
  facts <- c(facts, fact)
  
}


########



makemodels <- c()
prices <- c()
forhandler <- c()
adresse <- c()
cvr <- c()
linkz <- c()
specifikationer <- list()

bo <- cars %>% 
  html_elements("a.Listing_link__6Z504") %>% 
  html_attr("href")


#OBS få header på + sleep
for(page_num in 4:5){
  url <- paste0("https://www.bilbasen.dk/brugt/autocamper?fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&page=", page_num, "&sellertypes=dealer")
  res <- GET(url, add_headers("User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36"))
  page <- read_html(res)
  bo <- page %>% 
    html_elements("a.Listing_link__6Z504") %>% 
    html_attr("href")

for(i in 1:30){
  
  link2 <- read_html(bo[i])
  
  link3 <- link2 %>% html_elements("div.bas-MuiVipSectionComponent-sectionHeader.bas-MuiSellerInfoComponent-headerStyles")
  
  albert <- link2 %>% 
    html_elements("h1.bas-MuiTypography-root.bas-MuiCarHeaderComponent-title.bas-MuiTypography-h2") %>% 
    html_attr("title")
  
  gitte <- link2 %>% 
    html_elements("span.bas-MuiCarPriceComponent-value") %>% 
    html_text()
  
  jannik <- link3 %>% 
    html_elements("h2.bas-MuiTypography-root.bas-MuiTypography-h3") %>% 
    html_text()
  
  markus <- link2 %>% 
    html_elements("a.bas-MuiSellerInfoComponent-address") %>% 
    html_text()
  
  vera <- link2 %>% 
    html_elements("p.bas-MuiSellerInfoComponent-cvr") %>% 
    html_text()
  
  knud <- link2 %>% 
    html_elements("td.bas-MuiTableCell-root.bas-MuiTableCell-body.bas-MuiTableCell-alignRight") %>% 
    html_text()
  
  
  makemodels <- c(makemodels, albert)
  prices <- c(prices, gitte)
  forhandler <- c(forhandler, jannik)
  adresse <- c(adresse, markus)
  cvr <- c(cvr, gsub("[^0-9]","", vera))
  linkz <- c(links, bo)
  
  knudliste <- as.list(knud)
  specifikationer <- append(specifikationer, knudliste)
  
  
  #Sys.sleep(1)

}
  
}

for (i in 1:30) {
  specifikationer[[paste0("df_", i)]] <- knud
}


lst <- specifikationer   # your list of vectors

Find the maximum length
max_len <- max(lengths(lst))

Pad each vector with NA so all have the same length
padded <- lapply(lst, function(x) {
  c(x, rep(NA, max_len - length(x)))
})

Combine into a data frame
df <- as.data.frame(padded)

tdf <- as.data.frame(t(df))

#colname####
colname <- c("Modelår","1. registrering","Kilometertal","Drivmiddel",
             "Brændstofforbrug",
             "CO2 udledning",
             "Euronorm",
             "Periodisk afgift",
             "Ydelse",
             "Acceleration",
             "Tophastighed",
             "Geartype",
             "Antal gear",
             "Trækvægt",
             "Farve",
             "Nypris",
             "Kategori",
             "Type",
             "Bagagerumsstørrelse",
             "Vægt",
             "Bredde",
             "Længde",
             "Højde",
             "Lasteevne",
             "Max. trækvægt m/bremse",
             "Trækhjul",
             "Cylindre",
             "ABS-bremser",
             "ESP",
             "Airbags",
             "Tankkapacitet",
             "Døre")
#colname####

colnames(tdf) <- colname
tdfclean <- tdf[-(26:28),-(33)]
