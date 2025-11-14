#scrape make, model and price

library(rvest)
library(httr)
library(stringr)
library(dplyr)

makemodels <- c()
prices <- c()
facts <- c()
locations <- c()


url <- "https://www.bilbasen.dk/brugt/autocamper?includeengroscvr=true&includeleasing=false&sellertypes=dealer"
page <- read_html(url)
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



saveRDS(df, "C:\\Users\\marti\\Documents\\EK\\1. sem\\campersV1.rds")
?saveRDS


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

links <- cars %>% 
  html_elements("a.Listing_link__6Z504") %>% 
  html_attr("href")

forhandler <- c()
adresse <- c()
cvr <- c()


#OBS få header på + sleep
for(annonce in 1:30){
  
link2 <- read_html(links[annonce])

link3 <- link2 %>% html_elements("div.bas-MuiVipSectionComponent-sectionHeader.bas-MuiSellerInfoComponent-headerStyles")

  jannik <- link3 %>% 
  html_elements("h2.bas-MuiTypography-root.bas-MuiTypography-h3") %>% 
    html_text()
  
  markus <- link2 %>% 
    html_elements("a.bas-MuiSellerInfoComponent-address") %>% 
    html_text()
  
  vera <- link2 %>% 
    html_elements("p.bas-MuiSellerInfoComponent-cvr") %>% 
    html_text()
  
forhandler <- c(forhandler, jannik)
adresse <- c(adresse, markus)
cvr <- c(cvr, gsub("[^0-9]","", vera))

}
 
links 
 

  
  


