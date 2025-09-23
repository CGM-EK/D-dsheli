#Data transformation

library(readxl)
library(stringr)
FORV2 <- read_xlsx("FORV2.xlsx")

FORVkol1 <- seq.Date(from = as.Date("2000-01-01"),
                             to = as.Date("2025-09-30"),
                             by = "month")
FORVkol2 <- rep(FORV2$`Planer om køb af bil indenfor de næste 12 måneder?`, each=3)
FORVkol3 <- rep(FORV2$`Planer om køb eller opførsel af bolig indenfor de næste 12 måneder?`, each=3)
FORVkol4 <- rep(FORV2$`Planer om større forbedringer af eller renoveringer i hjemmet indenfor de næste 12 måneder?`, each=3)

FORB.INVEST <- data.frame(Month=(FORVkol1), 
                          `Planer om køb af bil indenfor de næste 12 måneder?`=rev(FORVkol2), 
                          `Planer om køb eller opførsel af bolig indenfor de næste 12 måneder?`=rev(FORVkol3),
                          `Planer om større forbedringer af eller renoveringer i hjemmet indenfor de næste 12 måneder?`=rev(FORVkol4)
                          )


