
library(rvest)
library(httr)
library(tidyverse)
library(tibble)
library(DBI)
library(RMariaDB)



#tomme lister
makemodels   <- c()
prices       <- c()
forhandler   <- c()
adresse      <- c()
cvr          <- c()
links        <- c()
beskrivelse  <- c()

specifikationer <- list()
#indeks for at gemme Knud
counter <- 1

#skal nok slettes
url <- "https://www.bilbasen.dk/brugt/autocamper?fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&sellertypes=dealer"
res <- GET(url, add_headers("User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36"))
page <- read_html(res)
cars <- page %>% html_nodes("article.Listing_listing__XwaYe")

#Scrapedate ##------ Tue Nov 18 14:47:31 2025 ------##


for(page_num in 1:5)
  #if statement som tager url for side 1 hvis page_num er lig 1 og ellers ændres page_num i URL.
  #Gøres da side 1 URL ikke kan have "Page=1"
{
  if(page_num == 1) {url <- "https://www.bilbasen.dk/brugt/autocamper?fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&sellertypes=dealer"
  } else 
  {
    url <- paste0("https://www.bilbasen.dk/brugt/autocamper?fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&page=", page_num, "&sellertypes=dealer")
  }
  res <- GET(url, add_headers("User-Agent"="Mozilla/5.0"))
  page <- read_html(res)
  
  #Siden over salgsopslag
  bo <- page %>% 
    html_elements("a.Listing_link__6Z504") %>% 
    html_attr("href")
  
  for(i in 1:length(bo)){
    #Henter de enkelte salgsopslag på siden
    link2 <- read_html(bo[i])
    
    #Gør scraping langsommere så vi forhåbentlig ikke bliver blokeret
    Sys.sleep(1.5)
    
    #Henter modelnavn 
    albert <- link2 %>% html_elements("h1.bas-MuiTypography-root.bas-MuiCarHeaderComponent-title.bas-MuiTypography-h2") %>% html_attr("title")
    #Henter salgspris
    gitte  <- link2 %>% html_elements("span.bas-MuiCarPriceComponent-value") %>% html_text()
    #Henter forhandler navn
    jannik <- link2 %>% html_elements("h2.bas-MuiTypography-root.bas-MuiTypography-h3") %>% html_text()
    #Henter forhandlers adresse
    markus <- link2 %>% html_elements("a.bas-MuiSellerInfoComponent-address") %>% html_text()
    #Henter forhandlers CVR
    vera   <- link2 %>% html_elements("p.bas-MuiSellerInfoComponent-cvr") %>% html_text()
    
    #Henter en liste af informationer om bilen
    knud <- link2 %>% 
      html_elements("td.bas-MuiTableCell-root.bas-MuiTableCell-body.bas-MuiTableCell-alignRight") %>% 
      html_text()
    
    #Henter beskrivelse på salgsannonce
    fatima <- link2 %>% html_elements("div.bas-MuiAdDescriptionComponent-descriptionText") %>% 
      html_elements("p") %>% 
      html_text()
    
    #Putter data ind i lister
    makemodels   <- c(makemodels, albert)
    prices       <- c(prices, gitte)
    forhandler   <- c(forhandler, jannik[6])
    adresse      <- c(adresse, markus)
    cvr          <- c(cvr, gsub("[^0-9]","", vera))
    links        <- c(links, bo[i])
    beskrivelse  <- c(beskrivelse, gsub("[^a-zA-Z0-9,æøå ÆØÅ ).(+-]", "", fatima)) 
    
    #Gemmer knud i en liste
    specifikationer[[counter]] <- knud
    counter <- counter + 1
  }
}

#Konvertere de enkelte bilers information til en "dropdown" i listen ved brug af tibblepakken og alle detaljer får sin egen linje.
specifikationer <- lapply(specifikationer, function(x) {
  tibble(specifikation = x)
})

# Inputter alle vectors i listen "specifikationer"
spec_vectors <- lapply(specifikationer, `[[`, "specifikation")

#Inputter NA værdier i tommefelter for at gøre dataframen lige lang i alle kolonner
max_len <- max(lengths(spec_vectors))
spec_padded <- lapply(spec_vectors, function(x) {
  c(x, rep(NA, max_len - length(x)))
})

# Laver til en dataframe
spec_df_wide <- as.data.frame(do.call(rbind, spec_padded))

#Opretter kolonnenavne
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

#Benytter colnames på dataframe
colnames(spec_df_wide) <- colname

#Cbinder salgsdetaljer til dataframen med informationer om bilen 
speccleanfinal <- cbind(spec_df_wide, adresse, cvr, forhandler, links, prices, makemodels, beskrivelse)

spfcolnames <- c("modelår","f_reg","km_tal","Drivmiddel",
                 "bs_forbrug",
                 "CO2",
                 "euronorm",
                 "periodisk_afg",
                 "ydelse",
                 "acceleration",
                 "tophastighed",
                 "geartype",
                 "antal_gear",
                 "trækvægt",
                 "farve",
                 "nypris",
                 "kategori",
                 "type",
                 "bag_str",
                 "vægt",
                 "bredde",
                 "længde",
                 "højde",
                 "lastevne",
                 "max_træk_m_b",
                 "trækhjul",
                 "cylindre",
                 "abs_brems",
                 "esp",
                 "airbags",
                 "tank_kap",
                 "døre",
                 "adresse",
                 "cvr",
                 "forhandler",
                 "links",
                 "prices",
                 "makemodels",
                 "beskrivelse"
                 ) 
speccleanfinal <- speccleanfinal[,-(33)]
colnames(speccleanfinal) <- spfcolnames
#Renser data ved at fjerne de få biler som havde et producent år.
speccleanfinal <- speccleanfinal[!grepl("/", speccleanfinal$km_tal), ]
#Fjerner tom kolonne

speccleanfinal <- speccleanfinal[-(95),]

########### 1.3
#scrapedate for nye biler ##------ Tue Nov 19 10:22:16 2025 ------##

#fjerner 5 biler
speccleanfinalminus5 <- speccleanfinal[-(1:5),]

#ændrer pris på de 3 første biler på side 1
speccleanfinalminus5[1:3,37] <- c("1.750.000 kr.", "625.000 kr.", "3.200.000 kr.")

#laver 2 nye "biler"
førstenyebile <-  c("1832", "-", "150.000 km", "Viljestyrke", "15,6 km/gulerod", "6 g/km", "1", "8.220 kr. / år",   "2 hk/-", "-", "25 km/t", "Manuel", "3", "2766 kg", "Hest brun - vogn guld", "4 rismark", "Heste Campingbus", "Karet", "-", "800 kg", "2 heste", "3 heste","1,5 heste", "Umenneskelige mængder i kg", "-",      
                    "Forhesttræk", "0", "Ja", "Nej", "4", "300 gulerødder", "2", "Prins Jørgens Gård 5, 1218 København", "12345678", "Jørgen", "hestenet.dk", "1.000 kr", "Ford hestevogn v6 Turbo", "Sej hestevogn kom og køb den")
andennyebil <- c("2012", "-", "200.000 km", "Børneben", "Afhængigt af barn", "2 g/km", "1", "220 kr. / år",   "1 bk/-", "-", "50 km/t", "Manuel", "3", "100 kg", "Gul og rød", "899,99 kr.", "Legetøjsbil", "Løbebil", "-", "22 kg", "60 cm", "100 cm", "80 cm", "Umenneskelige mængder i kg", "-",      
                 "Trækhjul", "0", "Ja", "Nej", "1", "2 børneben", "2", "Prins Jørgens Gård 5, 1218 København", "87654321", "Jørgen", "Børnevogn.dk", "450 kr.", "Børnevogn04", "Perfekt, køb og køb den")
#tilføjer de nye biler til dataframe
speccleanfinal1.3 <- rbind(speccleanfinalminus5, førstenyebile, andennyebil)
con <- dbConnect(MariaDB(),
                 host="localhost",
                 db="opg3",
                 user="root",
                 password=Sys.getenv("localpw"))

dbWriteTable(con,"autocamper",speccleanfinal1.3, overwrite=T)
dbWriteTable(con,"autocamper1",speccleanfinalminus5, overwrite=T)               
  