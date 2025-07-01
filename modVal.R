#

library(flexsurv)
library(survival)
source("Code/KMplot.R")

mod <- readRDS("Output/Models/flsm.rds")
espac3 <- readRDS("Data/espac3clean.rds")

mod_coef <- mod$coefficients
means <- mod$datameans

colnames(espac3)
mod_coef <- mod_coef[-grep('gamma', names(mod_coef))]
covs <- substr(names(mod_coef),1,4)

espac3_c <- espac3[,grepl(
  paste0(covs[1],"|",covs[2],"|",covs[3],"|",
         covs[4],"|",covs[5]),colnames(espac3))]

# https://www.statology.org/factor-to-numeric-in-r/
x <- sapply(espac3_c, is.factor)
espac3_c[,x] <- as.data.frame(apply(espac3_c[,x],2,as.numeric))

mod_coef
# lymph + resec + 
#diff + postop
# mod_coef[1]*(espac3_c[1,1]) + (mod_coef[2]* espac3_c[1,2]) + 
#   (mod_coef[3]*espac3_c[3,1]) + (mod_coef[5]*espac3_c[4,1])

espac3_c
#individual
#lets look at row 1 only:
mod_coef
means
espac3_c[1,]
# multiply coef by data val
0.4876152*(1) + (0.1805322*0) + (-0.4160534*0)+(0.2671471*(6.222576)) #2.149958
#multiply coef by data val mean centre ca19
0.4876152*(1) + (0.1805322*0) + (-0.4160534*0)+(0.2671471*(6.222576-3.3404581))
# multiply coef by all covariates mean centred
0.4876152*(1-0.7138643) + (0.1805322*(0-0.3923304)) + (-0.4160534*(0-0.6312684))+(0.2671471*(6.222576-3.3404581))
(mod_coef[1]*espac3_c[1,1]) + (mod_coef[2]* espac3_c[1,2]) + 
  (mod_coef[3]*espac3_c[1,3]) + (mod_coef[5]*espac3_c[1,4]) # 2.149958
# lypmh                           #resec
(0.4876152 *(espac3_c[1,1])) + (0.1805322* (espac3_c[1,2])) + 
  (-0.4160534*(espac3_c[1,3])) + (0.2671471*(espac3_c[1,4]-3.3404581)) # 1.257565

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
    
  espac3_c$lp[row] <- (coef_L*(espac3_c$LymphN[row]-(mean_L)))+
    (coef_r*(espac3_c$ResecM[row]-(mean_r)))+(coef_dif*(espac3_c$Diff_Status[row]-(mean_dif)))+
    (coef_ca19*(espac3_c$PostOpCA199[row] - mean_ca19))
  # espac3_c$m[row] <- 
}

espac3_c$pred <- predict(mod, type = 'lp')

range(espac3_c$pred$.pred_link)
# ?predict.flexsurvreg


#### cut lp into rgs
espac3_c$rg <- cut(espac3_c$pred$.pred_link, breaks = 4,
                   )

espac3_c$rg_2 <- cut(espac3_c$lp, breaks = 4)

espac3_c$stime <- espac3$stime
espac3_c$cen <- espac3$OS_cen


KMplot(time = espac3_c$stime, cen = espac3_c$cen, fac = espac3_c$rg,
       summStat=T,LRtest=F, ylab="Survival probability",
       xlab="Time",col=c("pink2","red","green3", "blue4"),lwd=4)

KMplot(time = espac3_c$stime, cen = espac3_c$cen, fac = espac3_c$rg_2,
       summStat=T,LRtest=F, ylab="Survival probability",
       xlab="Time",col=c("pink2","red","green3", "blue4"),lwd=4)
