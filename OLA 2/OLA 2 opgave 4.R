#loader pakker til brug i opgaven
library(readxl)
library(danstat)
library(dkstat)
library(mapDK)
library(tidyverse)
#loader data for forbrugertillid og privatforbrug til brug i opgaven
f.tillid <- read_excel("R/R projekter/Forbrugertillidsindikator2000_2025 OLA2.xlsx", 
                       sheet = "Ark1")

#laver et gennemsnit på vektoren
mean(f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)
#-10.53072

year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
plotdata <- as.data.frame(year)

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerplot1 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq1]
kvartalerplot2 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq2]
kvartalerplot3 <- f.tillid$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`[kvartalseq3]
plotdata$ansk <- c((kvartalerplot1+kvartalerplot2+kvartalerplot3)/3)

ggplot(data = plotdata, aes(x=year, y = ansk))+
  geom_line()+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_x_date(name = "Year", breaks = plotdata$year[seq(1, length(plotdata$year), by = 4)],
               labels = format(plotdata$year[seq(1, length(plotdata$year), by = 4)], "%Y"))

########################3

dkbefolkningpost <- dst_meta(table = "FU13", lang = "da")

dkforbrug_meta_filters <- list(
  KONSUMGRP = c("01 FØDEVARER OG IKKE-ALKOHOLISKE DRIKKEVARER",
  "02 ALKOHOLISKE DRIKKEVARER, TOBAK OG EUFORISERENDE STOFFER",
  "03 BEKLÆDNING OG FODTØJ",
  "04 BOLIG, VAND, ELEKTRICITET, GAS OG ANDET BRÆNDSEL",
  "05 BOLIG-, HUSHOLDNINGSUDSTYR SAMT HUSHOLDNINGSTJENESTER",
  "06 SUNDHED",
  "07 TRANSPORT",
  "08 INFORMATION OG KOMMUNIKATION",
  "09 FRITID, SPORT OG KULTUR",
  "10 UNDERVISNING",
  "11 RESTAURANTER OG HOTELLER",
  "12 FORSIKRING OG FINANSIELLE TJENESTEYDELSER",
  "13 PERSONLIG PLEJE SAMT DIVERSE VARER OG TJENESTER"),
  HUSSTAND = "Gennemsnitshusstand",
  PRISENHED = "Faste priser",
  Tid = "*"
)
befolkningsdata <- dst_get_data(table = "FU13", query = dkforbrug_meta_filters, lang = "da")
befolkningsdatacl <- befolkningsdata

ggplot(data = befolkningsdata, aes(x=TID, y=value, group = KONSUMGRP, color = KONSUMGRP))+
  geom_line()

#vi laver plottet igen men med indekstal for at gøre udviklingen visuelt sammenlignelig
forbrugsgoder_2020_2025 <- read_excel("R/forbrugsgoder 2020-2025.xlsx", 
                                      sheet = "Ark1")
dfindeks <- forbrugsgoder_2020_2025 %>% mutate(indeks = row_number())

forbrugsgoder_2020_2025 <- forbrugsgoder_2020_2025 %>%
  mutate(across(where(is.numeric),
                ~ .x / first(.x) * 100,
                .names = "{.col}_idx"))          

