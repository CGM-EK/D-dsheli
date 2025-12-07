library(tidyverse)

#hiver kolonnen ud for lånemuligheder og ændrer dem til 3 variabler istedet for 5
ju <- df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`
ju[v == "Dårlig"] <- "Dårlige"
nyemuligheder <- case_when(
  v %in% c("Meget dårlige", "Dårlige") ~ "Dårlige(n=447)",
  v %in% c("Neutrale") ~ "Neutrale(n=800)",
  v %in% c("Gode", "Meget gode") ~ "Gode(n=3186)",
  TRUE ~ NA_character_
)
dflol <- df
dflol$lånmuligheder <- nyemuligheder
#fjerner naværdierne fra kolonnen eks der hvor der stod "ved ikke" før
df_lol <- dflol[!is.na(dflol$lånmuligheder), ]
table(dflol$lånmuligheder)
#Definerer rækkefølgen
levels_orderny <- c(
  "Dårlige(n=447)",
  "Neutrale(n=800)",
  "Gode(n=3186)"
)

#Laver til faktor
dflol$lånmuligheder <- factor(y_new, levels = levels_orderny, ordered = TRUE)

ggplot(data = df_lol, aes(x=lånmuligheder,
                      fill = lånmuligheder))+
  geom_bar()+
  theme_minimal()+
  labs(
    title = "Størstedelen af udspurgte virksomheder ser favorabelt på deres lånemuligheder",
    y = "Antal",
    x = "Svarkategori"
  )
table(df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`)
table(df_clean$lånemuligheder)

dfuvi <- df[!grepl("Ved", df$`Hvordan ser du mulighederne for at låne penge til din virksomhed? (fiktivt spørgsmål)`), ]

################3

summary(df$`Antal ansatte Cvr-nr.`)

mybreaks=c(10000, 500, 100,50, 20, 0)
mylabs=c("1-20 ansatte", "21-50 ansatte", "51-100 ansatte", "101-500 ansatte", "Over 500 ansatte")

#den nye kategorivariabel defineres
ansattekategori <- cut(df$`Antal ansatte Cvr-nr.`, labels = mylabs, breaks = mybreaks)

ansattekategori
table(ansattekategori)
df$ansattekategori <- ansattekategori

#### vi prøver at indekse talene

table(df$lånemuligheder)
jonathan <- subset(df,df$lånemuligheder=="Dårlige")
jannik <- subset(df,df$lånemuligheder=="Neutrale")
jakob <- subset(df,df$lånemuligheder=="Gode")
jurl <- as.vector(table(jakob$ansattekategori))
gode <- as.numeric(jurl/nrow(jakob)*100)
kurl <- as.vector(table(jonathan$ansattekategori))
dårlige <- as.numeric(kurl/nrow(jonathan)*100)
qurl <- as.vector(table(jannik$ansattekategori))
neutrale <- as.numeric(qurl/nrow(jannik)*100)
dfplot

dfplot <- data.frame(cbind(gode,neutrale,dårlige, mylabs))


df_plot <- dfplot %>%
  pivot_longer(
    cols = c(gode, neutrale, dårlige),
    names_to = "kategori",
    values_to = "værdi"
  )
df_plot$værdi <- as.numeric(df_plot$værdi)
levels_orderigen <- c(
  "1-20 ansatte",
  "21-50 ansatte",
  "51-100 ansatte",
  "101-500 ansatte",
  "Over 500 ansatte"
)

#Lav til faktor
df_plot$mylabs <- factor(df_plot$mylabs, levels = levels_orderigen, ordered = TRUE)

nyelevels <- c(
  "dårlige",
  "neutrale",
  "gode"
)
df_plot$kategori <- factor(df_plot$kategori, levels = nyelevels, ordered = TRUE)

ggplot(df_plot, aes(x = mylabs, y = værdi, fill = kategori)) +
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,100))+
  labs(title = "Flest af de større virksomheder vurderes at de har gode lånemuligheder",
       x= "indeks for lånemuligheder",
       y= "ansattekategori")+
  theme_minimal()
