#oprettelse af faktorvariabel der viser om det kvartalvise årlige vækst er steget eller faldet

dataframe1log <- data.frame(forbrugertillidDI$kvartaler)
dataframe1log$pfv <- forbrugertillidDI$pfv
dataframe1log$opned <- as.factor(ifelse(dataframe1log$pfv >=0, "1", "0"))
table(dataframe1log$opned)
# no yes 
# 24 78 

#
dataframe1log$DIft <- forbrugertillidDI$`c((kvartalerDIft1 + kvartalerDIft2 + kvartalerDIft3)/3)`
dataframe1log$opnedDI <- as.factor(ifelse(dataframe1log$DIft >=0, "1", "0"))
dataframe1log$DSTft <- forbrugertillidDST
dataframe1log$opnedDST <- as.factor(ifelse(dataframe1log$DSTft >=0, "1", "0"))

glm.DIPFV <- glm(formula = dataframe1log$opned~dataframe1log$opnedDI, family = "binomial")
summary(glm.DIPFV)

glm.DSTPFV <- glm(formula = dataframe1log$opned~dataframe1log$opnedDST, family = "binomial")
summary(glm.DSTPFV)


#
dataframe2log <- data.frame(dataframe1log$pfv)
dataframe2log$opned <- dataframe1log$opned
dataframe2log$dstft <- dataframe1log$DSTft
dataframe2log$dift <- dataframe1log$DIft

dataframeoplog <- subset(dataframe2log, dataframe2log$opned == 1)
dataframenedlog <- subset(dataframe2log, dataframe2log$opned == 0)

gennemsnitopDST <- mean(dataframeoplog$dstft)
gennemsnitopDI <- mean(dataframeoplog$dift)

gennemsnitnedDST <- mean(dataframenedlog$dstft)
gennemsnitnedDI <- mean(dataframenedlog$dift)

gennemsnitDST <- mean(dataframe1log$DSTft)
gennemsnitDI <- mean(dataframe1log$DIft)
gennemsnitvækst <- mean(dataframe1log$pfv)
