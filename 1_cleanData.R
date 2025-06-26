library(mice)


setwd("~/Documents/GitHub/PDAC")

espac3_L <- read.csv("Data/espac3.csv", row.names = 1)
# dat <- read.csv("Data/espac3_clean.csv")

espac3 <- espac3_L[espac3_L$Arm == "GEM" &espac3_L$TumourType=="Ductal",]


# calc surv time
espac3$stime <- as.numeric(difftime(as.Date(espac3$OS_dt_2), as.Date(espac3$Surg_dt), units = "days")/30.44)


espac3 <- espac3[,which(colnames(espac3) %in% c("LymphN","Diff_Status","PostOpCA199","ResecM","WHO","Diabetic","LocalInv",
                                                "stime", "OS_cen","MaxTumSiz"))]


# retain patients with baseline measure of ca199
# espac3 <- espac3[-which(is.na(espac3$PostOpCA199)),]
espac3 <- espac3[complete.cases(espac3),]


espac3$PostOpCA199 <- log(espac3$PostOpCA199+1)
espac3$MaxTumSiz <- sqrt(espac3$MaxTumSiz)
espac3$LymphN <- as.factor(espac3$LymphN)
espac3$Diff_Status <- as.factor(espac3$Diff_Status)
espac3$PostOpCA199 <- as.numeric(espac3$PostOpCA199)
espac3$OS_cen <- as.numeric(espac3$OS_cen)

espac3 <- espac3[-which(espac3$Diff_Status == "Undifferentialted"),]


espac3$Diff_Status <- droplevels(espac3$Diff_Status)

espac3[1:3,]

saveRDS(espac3,  "Data/espac3clean.rds")


