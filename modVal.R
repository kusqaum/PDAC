#

library(flexsurv)
library(survival)
source("Code/KMplot.R")

mod <- readRDS("Output/Models/flsm.rds")
espac3 <- readRDS("Data/espac3clean.rds")

mod_coef <- mod$coefficients
means <- mod$datameans
knot <- mod$knots
lam <- ((max(knot)-knot[2]))/((max(knot)-min(knot)))
gam <- mod$coefficients[grep("gamma", names(mod$coefficients))]
mod_coef <- mod_coef[-grep('gamma', names(mod_coef))]
covs <- substr(names(mod_coef),1,4)


espac3_c <- espac3[,grepl(
  paste0(covs[1],"|",covs[2],"|",covs[3],"|",
         covs[4],"|",covs[5]),colnames(espac3))]

# https://www.statology.org/factor-to-numeric-in-r/
x <- sapply(espac3_c, is.factor)
espac3_c[,x] <- as.data.frame(apply(espac3_c[,x],2,as.numeric))


espac3_c$lp <- NULL

for(row in 1:nrow(espac3_c)){
  #coeff <- mod_coef
  # for (col in 1:ncol(espac3_c)) {
  if(espac3_c$Diff_Status[row]==1|espac3_c$Diff_Status[row]==0){
    coeff <- mod_coef
    coef_L <- coeff[1]
    coef_r <- coeff[2]
    coef_dif <- coeff[3]
    coef_ca19 <- coeff[5]
    mean_L <- means[1]
    mean_r <- means[2]
    mean_dif <- means[3]
    mean_ca19 <- means[5]
  }
  if(espac3_c$Diff_Status[row]==2){
    coeff <- mod_coef
    coef_L <- coeff[1]
    coef_r <- coeff[2]
    coef_dif <- coeff[4]
    coef_ca19 <- coeff[5]
    mean_L <- means[1]
    mean_r <- means[2]
    mean_dif <- means[4]
    mean_ca19 <- means[5]
  }
    
  # espac3_c$lp[row] <- (coef_L*(espac3_c$LymphN[row]-(mean_L)))+
  #   (coef_r*(espac3_c$ResecM[row]-(mean_r)))+(coef_dif*(espac3_c$Diff_Status[row]-(mean_dif)))+
  #   (coef_ca19*(espac3_c$PostOpCA199[row] - mean_ca19))
  espac3_c$lp[row] <- (coef_L*(espac3_c$LymphN[row]))+
    (coef_r*(espac3_c$ResecM[row]))+(coef_dif*(espac3_c$Diff_Status[row]))+
    (coef_ca19*(espac3_c$PostOpCA199[row])) 
}

#change back to fac
espac3_c$LymphN <- as.factor(espac3_c$LymphN)
espac3_c$ResecM <- as.factor(espac3_c$ResecM)
espac3_c$Diff_Status <- as.factor(espac3_c$Diff_Status)

eta <- rowSums(basis(knot, log(espac3$stime)) * gam)

espac3_c$lp_eta <- eta+espac3_c$lp
espac3_c$pred <- predict(mod, type = 'lp')

range(espac3_c$pred$.pred_link)
# ?predict.flexsurvreg





#### cut lp into rgs ####
espac3_c$rg <- cut(espac3_c$lp_eta, breaks = 4,
                   labels = c("1","2","3","4"))

lp_q <- quantile(espac3_c$pred$.pred_link, c(0.25,0.5,0.75))
espac3_c$rg <- cut(espac3_c$pred$.pred_link, breaks = c(-Inf, lp_q, Inf), 
                  labels = c("g1","g2","g3","g4"))

# espac3_c$rg_2 <- cut(espac3_c$lp, breaks = 4)

espac3_c$stime <- espac3$stime
espac3_c$cen <- espac3$OS_cen


KMplot(time = espac3_c$stime, cen = espac3_c$cen, fac = espac3_c$rg,
       summStat=T,LRtest=F, ylab="Survival probability",
       xlab="Time",col=c("pink2","red","green3", "blue4"),lwd=4)

sob <- Surv(espac3_c$stime, espac3_c$cen)
concordance(sob ~ rg, data = espac3_c)
coxph(sob ~rg, data = espac3_c)
KMplot(time = espac3_c$stime, cen = espac3_c$cen, fac = espac3_c$rg_2,
       summStat=T,LRtest=F, ylab="Survival probability",
       xlab="Time",col=c("pink2","red","green3", "blue4"),lwd=4)
sfRG <- survfit(Surv(stime,cen)~rg, data = espac3_c)
plot(sfRG, col = c(1,2,3,4))

lp <- predict(mod)
summary(lp)
# 
