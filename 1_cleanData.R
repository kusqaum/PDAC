library(mice)



espac3_L <- read.csv("Data/espac3.csv", row.names = 1)
dat <- read.csv("Data/espac3_clean.csv")

espac3 <- espac3_L[espac3_L$Arm == "GEM",]
espac3_gem <- espac3_L[espac3_L$Arm == "GEM",]

# calc surv time
espac3$stime <- as.numeric(difftime(as.Date(espac3$OS_dt_2), as.Date(espac3$Surg_dt), units = "days")/30.44)

range(espac3$stime, na.rm = T)

espac3 <- espac3[,which(colnames(espac3) %in% c("LymphN","Stage","Diff_Status","PostOpCA199", 
                                                  "stime", "OS_cen"))]

# retain patients with baseline measure of ca199
espac3 <- espac3[-which(is.na(espac3$PostOpCA199)),]

md.pattern(espac3)

espac3$PostOpCA199 <- log(espac3$PostOpCA199+1)
espac3$LymphN <- as.factor(espac3$LymphN)
espac3$Diff_Status <- as.factor(espac3$Diff_Status)
espac3$Stage <- as.factor(espac3$Stage)
espac3$PostOpCA199 <- as.numeric(espac3$PostOpCA199)
espac3$OS_cen <- as.numeric(espac3$OS_cen)

espac3 <- espac3[-which(espac3$Diff_Status == "Undifferentialted"),]
espac3$Diff_Status<- droplevels(espac3$Diff_Status)
# espac3 <- espac3[-which(espac3$Stage == 4)]
# espac3p <- espac3[complete.cases(espac3),]

set.seed(882)
espac3_mice <- mice(espac3)
espac3_imp <- complete(espac3_mice)

saveRDS(espac3_imp,  "Data/espac3clean.rds")

# merge with other dataset

espac3m <- merge(espac3_gem, dat, by = "PatID")
espac3m <- espac3m[,which(colnames(espac3m) %in% c("LymphN.x","Stage","Diff_Status"
                                        ,"PostOpCA199.x", "stime",
                                        "OS_cen"))]

length(which(is.na(espac3m$PostOpCA199.x)))
espac3m <- espac3m[-which(is.na(espac3m$PostOpCA199)),]
espac3m <- espac3m[-which(espac3m$Diff_Status == "Undifferentialted"),]

espac3m$LymphN.x <- as.factor(espac3m$LymphN.x)
espac3m$Stage <- as.factor(espac3m$Stage)
espac3m$Diff_Status <- as.factor(espac3m$Diff_Status)
espac3m$Diff_Status<- droplevels(espac3m$Diff_Status)
espac3m$PostOpCA199.x <- as.numeric(espac3m$PostOpCA199.x)
espac3m$OS_cen <- as.numeric(espac3m$OS_cen)
espac3m$PostOpCA199.x <- log(espac3m$PostOpCA199.x+1)

set.seed(882)
imp_mer <- mice(espac3m)
imp_mer <- complete(imp_mer)

saveRDS(imp_mer, "Data/mergedEspac.rds")

# length(which(espac3$Stage==1))

