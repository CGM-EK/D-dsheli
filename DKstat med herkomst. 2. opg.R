library(dkstat)
library(ggplot2)

FODIE<- dst_meta(table = "FODIE", lang = "da")

vv <- FODIE

FODIE_meta_filters <- list(
  OMRÅDE = "Hele landet",
  MOOPRIND = c("Iran", "Irak", "Libyen", "Pakistan", "Somalia", "Syrien", "Tyrkiet"),
  MODERSALDER = "*",
  Tid = "*"
)

Fodiedata <- dst_get_data(table = "FODIE", query = FODIE_meta_filters, lang = "da")

ggplot(Fodiedata, aes(TID, value, colour=HERKOMST))+
  geom_line()+
  geom_point()

FODIE[["values"]][["MODERSALDER"]][["text"]][1:8]
