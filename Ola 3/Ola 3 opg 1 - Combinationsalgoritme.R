#loader pakker til brug i opgaven
library(tidyverse)
library(ggplot2)

#vi henter data fra Danmarks statistik
forbrugerforv <- dst_meta(table = "FORV1", lang = "da")

#vi udvælger variabler vi vil kigge på og opretter et dataset
forbrugerforv_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)
f.tillid1 <- dst_get_data(table = "FORV1", query = forbrugerforv_meta_filters, lang = "da")
f.tillid1 <- f.tillid1 %>% filter(TID >="2000-01-01")

f.tillid <- pivot_wider(
  data = f.tillid1,
  names_from = INDIKATOR,
  values_from = value)

#kvartalersekvenser opsættes
kvartalseq1 <- seq(1,304, 3)
kvartalseq2 <- seq(2,305, 3)
kvartalseq3 <- seq(3,306, 3)

#kvartalsekvenser anvendes på forbrugertillidsindikatorerne og der oprettes dataframes
kvartalerft1 <- f.tillid[c(kvartalseq1),3:ncol(f.tillid)]
kvartalerft2 <- f.tillid[c(kvartalseq2),3:ncol(f.tillid)]
kvartalerft3 <- f.tillid[c(kvartalseq3),3:ncol(f.tillid)]
forbrugertillid <- as.data.frame(c((kvartalerft1+kvartalerft2+kvartalerft3)/3))

#der oprettes en vektorer for kvartalerne fra k1 2000 til k2 2025
year <- seq.Date(from = as.Date("2000-01-01"),
                 to = as.Date("2025-06-30"),
                 by = "quarter")
f.tillidsammen <- as.data.frame(year)

###TESTER###
comb5 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb7 <- combn(x = forbrugertillid[1,1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)
comb9 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=1,FUN = NULL, simplify=F)
comb10 <- combn(x = forbrugertillid[1:nrow(forbrugertillid),1:ncol(forbrugertillid)],m=2,FUN = NULL, simplify=F)

#Looper
comb <- vector("list", ncol(forbrugertillid) - 1)

for (j in 1:(ncol(forbrugertillid) - 1)) {
  comb[[j]] <- vector("list", nrow(forbrugertillid))

  for (i in 1:nrow(forbrugertillid)) {
    # Generér kombinationer af kolonnenavne
    combo_names <- combn(colnames(forbrugertillid), m = j, simplify = TRUE)

    # Generér de faktiske data
    temp <- combn(
      x = as.numeric(forbrugertillid[i, ]),
      m = j,
      simplify = TRUE
    )

    # Sørg for at få en matrix
    if (is.null(dim(temp))) temp <- matrix(temp, nrow = j)

    # Beregn gennemsnit
    means <- colMeans(temp)

    # Tilføj navne som attribut
    names(means) <- apply(combo_names, 2, paste, collapse = "_")

    comb[[j]][[i]] <- means
  }
}


