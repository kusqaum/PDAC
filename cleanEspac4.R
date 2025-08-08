# read in dataset
espac4 <- read.csv("Data/panc_e4.csv", row.names = 1)

colnames(espac4)[which(colnames(espac4)=="R")] <- "ResecM"
colnames(espac4)[which(colnames(espac4)=="N")] <- "LymphN"
colnames(espac4)[which(colnames(espac4)=="Diff")] <- "Diff_Status"

espac4$ResecM[espac4$ResecM=="Pos"] <-1
espac4$ResecM[espac4$ResecM=="Neg"] <-0
espac4$LymphN[espac4$LymphN=="Pos"] <-1
espac4$LymphN[espac4$LymphN=="Neg"] <-0
espac4$Diff_Status[espac4$Diff_Status==1] <- 0
espac4$Diff_Status[espac4$Diff_Status==2] <- 1
espac4$Diff_Status[espac4$Diff_Status==3] <- 2
colnames(espac4)[which(colnames(espac4)=="stime")] <-"time"

espac4$ResecM <- as.factor(espac4$ResecM);espac4$LymphN <- as.factor(espac4$LymphN); espac4$Diff_Status<-as.factor(espac4$Diff_Status)

#data for external validation:
espac4 <- espac4[which(colnames(espac4) %in% c("LymphN", "ResecM", "Diff_Status", "PostOpCA199", "time","cen", "treat"))]
espac4$PostOpCA199 <- log(espac4$PostOpCA199+1)

espac4_gem <-espac4[which(espac4$treat=="GEM"),]
espac4_gemcap <- espac4[which(espac4$treat=="GEMCAP"),] # 365 patients
espac4_gemcap <- espac4_gemcap[complete.cases(espac4_gemcap),] # 362 (3 patients removed)



e4_gem <- espac4_gem[,which(colnames(espac4_gem)%in% c("LymphN", "ResecM", "Diff_Status", "PostOpCA199"))]
e4_gem$ResecM1 <- ifelse(e4_gem$ResecM==1, 1, 0)
e4_gem$LymphN1 <- ifelse(e4_gem$LymphN==1, 1, 0)
e4_gem$Diff_Status1 <- ifelse(e4_gem$Diff_Status==1,1,0)
e4_gem$Diff_Status2 <- ifelse(e4_gem$Diff_Status==2,1,0)

sobj <- Surv(espac4_gem$time, espac4_gem$cen)
e4_cov <- e4_gem[,5:8]
e4_cov$PostOpCA199 <- e4_gem$PostOpCA199
e4_cov$s.obj <- sobj
# espac4_gem <- espac4_gem[,which(colnames(espac4_gem) %in% c("LymphN", "ResecM", "Diff_Status", "PostOpCA199", "time","cen"))]


save(espac4_gem,file = "Data/espac4gem.R")
save(espac4_gemcap, file = "Data/espac4gemcap.R")
save(e4_cov, file = "Data/e4_cov.R")

