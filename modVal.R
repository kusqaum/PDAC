#
library(flexsurv)
mod <- readRDS("Output/Models/flsm.rds")
espac3 <- readRDS("Data/espac3clean.rds")

mod_coef <- mod$coefficients

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

data <- espac3_c
(mod_coef[1]*data[1,1]) + (mod_coef[2]* data[1,2]) + 
  (mod_coef[3]*data[1,3]) + (mod_coef[5]*data[1,4]) # 1.245526
means <- flsm$datameans
data$rg <- NULL
data$m <- NULL
for(row in 1:nrow(data)){
  #coeff <- mod_coef
  # for (col in 1:ncol(data)) {
  if(data$Diff_Status[row]==1|data$Diff_Status[row]==0){
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
  if(data$Diff_Status[row]==2){
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
    
  data$rg[row] <- (coef_L*data$LymphN[row])+(coef_r*data$ResecM[row])+(coef_dif*data$Diff_Status[row])+(coef_ca19*data$PostOpCA199[row])
  #data$m[row] <- (coef_L*0)+(coef_r*0)+(coef_dif*0)+(coef_ca19*mean_ca19)  
    # if(data$Diff_Status[row]==0){
    #   coef_L <- mod_coef[1]
    #   coef_r <- mod_coef[2]
    #   coef_dif <- mod_coef[4]
    #   coef_ca19 <- mod_coef[5]
    #   data$rg <- (coef_L*data[row,col])+(coef_r*data[row,col])+(coef_dif*data[row,col])+(coef_ca19*data[row,col])
    # }
    #if(){
      
    #}
  
}
# data$lp <- data$rg - data$m

data$pred <- predict(flsm, type = 'lp')


