
espac3_L <- read.csv("Data/espac3.csv", row.names = 1)
espaccl<- read.csv("Data/espac3_clean.csv")

espac3_L <- espac3_L[espac3_L$Arm == "GEM",]
espac3_L <- espac3_L[espac3_L$TumourType == "Ductal",]

espac3 <- espac3_L[, -c(2,3,5,6,7,8,9,10,11,13,14,15,21,22,25,26,28,
                        29,30,31,35,37,38,39,42,43,45,46,47,48,49,50,
                        51,53,54,55,56)]

# calc surv time
espac3$stime <- as.numeric(difftime(as.Date(espac3$OS_dt_2), as.Date(espac3$Day1trt), units = "days")/30.44)

range(espac3$stime, na.rm = T)

espac3<- espac3[-which(is.na(espac3$stime)),]

espac3 <- espac3[,!colnames(espac3) %in% c("EOT_Dt","FUdt", "TRTdt", "CEN2", "Day1trt", "OS_dt",
                                           "Death_dt", "Censor_dt", "OS_dt_2", "Prog_dt",
                                           "PFS_dt", "PFS_dt_2", "Prog_cen","PFS_cen", "Arm")]

summary(espac3)
espac3$Sex <- as.factor(espac3$Sex)
espac3$LymphN <- as.factor(espac3$LymphN)
espac3$ResecM <- as.factor(espac3$ResecM)
espac3$Rstatus <- as.factor(espac3$Rstatus)
espac3$Stage <- as.factor(espac3$Stage)
espac3$WHO <- as.factor(espac3$WHO)
espac3$Smoke <- as.factor(espac3$Smoke)
espac3$ConMedCond <- as.factor(espac3$ConMedCond)
espac3$Diabetic <- as.factor(espac3$Diabetic)
espac3$LocalInv <- as.factor(espac3$LocalInv)
espac3$PreOpCA199 <- as.numeric(espac3$PreOpCA199)
espac3$PostOpCA199 <- as.numeric(espac3$PostOpCA199)
espac3$MaxTumSiz <- as.numeric(espac3$MaxTumSiz)
espac3$OS_cen <- as.numeric(espac3$OS_cen)
espac3$PostOpCA199 <- log(espac3$PostOpCA199+1)

espac3_2$PostOpCA199 <- log(espac3_2$PostOpCA199+1)
espac3_2$MaxTumSiz <- sqrt(espac3_2$MaxTumSiz)






