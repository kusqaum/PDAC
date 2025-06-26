library(flexsurv)


getwd()

library(ggpubr)
# read data
espac3 <- readRDS("Data/espac3clean.rds")
espac_merged <- readRDS("Data/mergedEspac.rds")



flsm <- flexsurvspline(Surv(stime, OS_cen) ~ PostOpCA199+ LymphN+
                       Stage+ Diff_Status,
                       data = espac3, k = 3)

flsm


table(espac3$Stage)
table(espac3$Diff_Status)
table(espac3$LymphN)

setwd("/Users/richardjackson/Documents/GitHub/pscRepository/Models/PDAC/Gem_model")

load("CFM.Rds")
dir()

ggarrange(plotlist=CFM$datavis)



flsm2 <- flexsurvspline(Surv(stime, OS_cen) ~ PostOpCA199.x + LymphN.x +
                          Diff_Status + Stage,
                       data = espac_merged, k = 3)




plot(flsm)
plot(flsm2)
