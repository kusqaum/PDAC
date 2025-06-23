library(flexsurv)

# read data
espac3 <- readRDS("Data/espac3clean.rds")
espac_merged <- readRDS("Data/mergedEspac.rds")

flsm <- flexsurvspline(Surv(stime, OS_cen) ~ PostOpCA199+ LymphN+
                       Stage+ Diff_Status,
                       data = espac3, k = 3)

flsm
flsm2 <- flexsurvspline(Surv(stime, OS_cen) ~ PostOpCA199.x + LymphN.x +
                          Diff_Status + Stage,
                       data = espac_merged, k = 3)



flsm2

plot(flsm)
plot(flsm2)
