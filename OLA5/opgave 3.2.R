library(tidyverse)
library(ordinal)
library(readxl)

#loader dataset til opgaven
regnskaber_industri_transport_byg_5_25000_ansatte_anonym <- read_excel("R/R projekter/FLOW 3/regnskaber_industri_transport_byg_5_25000_ansatte_anonym.xlsx")
View(regnskaber_industri_transport_byg_5_25000_ansatte_anonym)

#laver en lidt mere bearbejdlig dataframe til brug i opgaven ud fra datasættet
df <- regnskaber_industri_transport_byg_5_25000_ansatte_anonym[c(1,2,5,6,194,195,196,197,198,206,207,208,209,210,212,213,214,215,216,218,219,220,221,222,224,225,226,227,228)]

#tjekker de forskellige besvarelser i spørgeskemaet
unique(df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)

# Kopiér kolonnen til et kort navn
v <- df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`

#retter stavefejl / variationer
v[v == "Dårlig"] <- "Dårlige"
x_new <- case_when(
  v %in% c("Meget dårlige", "Dårlige") ~ "Dårlige",
  v %in% c("Neutrale") ~ "Neutrale",
  v %in% c("Gode", "Meget gode") ~ "Gode",
  TRUE ~ NA_character_
)
unique(x_new)

# 2. Definér rækkefølgen
levels_order <- c(
  "Dårlige",
  "Neutrale",
  "Gode"
)

# 3. Lav til faktor
df$lånemuligheder <- factor(x_new, levels = levels_order, ordered = TRUE)
df_clean <- df[!is.na(df$lånemuligheder), ]
levels(df_clean$lånemuligheder)
unique(df_clean$lånemuligheder)

###############opgave 2####################
modfuld <- clm(df$lånemuligheder ~ log(df$`Balance 2020 (1.000 kr)`)+df$`Afkastningsgrad 2020 (%)` + 
              df$`Likviditetsgrad 2020 (%)` + df$`Soliditetsgrad 2020 (%)` + 
              df$`Egenkapital forrentning 2020 (%)`, data = df, link = "logit")
summary(modfuld)

mod1 <- clm(df$lånemuligheder ~ log(df$`Balance 2020 (1.000 kr)`), data = df, link = "logit")
summary(mod1)
mod2 <- clm(df$lånemuligheder ~ df$`Afkastningsgrad 2020 (%)`, data = df, link = "logit")
summary(mod2)
mod3 <- clm(df$lånemuligheder ~ df$`Likviditetsgrad 2020 (%)`, data = df, link = "logit")
summary(mod3)
mod4 <- clm(df$lånemuligheder ~ df$`Soliditetsgrad 2020 (%)`, data = df, link = "logit")
summary(mod4)
mod5 <- clm(df$lånemuligheder ~ df$`Egenkapital forrentning 2020 (%)`, data = df, link = "logit")
summary(mod5)


################opgave 3#####################
#se plot scripts