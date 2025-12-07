#pga problemer med hentning som csv er filen blevet hentet som excel
library(readxl)
regnskaber_industri_transport_byg_5_25000_ansatte_anonym <- read_excel("R/R projekter/FLOW 3/regnskaber_industri_transport_byg_5_25000_ansatte_anonym.xlsx")
View(regnskaber_industri_transport_byg_5_25000_ansatte_anonym)


df <- regnskaber_industri_transport_byg_5_25000_ansatte_anonym[c(1,2,5,6,194,206,207,208,209,210,212,213,214,215,216,218,219,220,221,222,224,225,226,227,228)]


library(ordinal)

unique(df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)
# Kopiér kolonnen til et kort navn
v <- df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`

# 1. Ret stavefejl / variationer
v[v == "Dårlig"] <- "Dårlige"
x_new <- dplyr::case_when(
  v %in% c("Meget dårlige", "Dårlige") ~ "Dårlige",
  v %in% c("Neutrale") ~ "Neutrale",
  v %in% c("Gode", "Meget gode") ~ "Gode",
  TRUE ~ NA_character_
)
unique(x_new)
v# 2. Definér rækkefølgen (fra mest negative til mest positive)
levels_order <- c(
  "Dårlige",
  "Neutrale",
  "Gode"
)

# 3. Lav til faktor
df$lånemuligheder <- factor(x_new, levels = levels_order, ordered = TRUE)


df_clean <- df[!is.na(df$lånemuligheder), ]
# Tjek resultatet
levels(df$lånemuligheder)
unique(df$lånemuligheder)

library(ggplot2)
mod1 <- clm(df$lånemuligheder ~ log(df$`Balance 2020 (1.000 kr)`)+df$`Afkastningsgrad 2020 (%)` + 
              df$`Likviditetsgrad 2020 (%)` + df$`Soliditetsgrad 2020 (%)` + 
              df$`Egenkapital forrentning 2020 (%)`, data = df, link = "logit")
summary(mod1)

library(tidyverse)
unique(df$`Branchebetegnelse primær`)
unique(df$`Branchekode primær`)
