library(flexsurv)
library(survival)
library(MASS)
getwd()

library(ggpubr)
# read data
espac3 <- readRDS("Data/espac3clean.rds")
colnames(espac3)[which(colnames(espac3)=="stime")]<-"time"
colnames(espac3)[which(colnames(espac3)=="OS_cen")] <- "cen"
survfit(Surv(time,cen)~1, data=espac3)

# first cox model w// all covars
cmFull <- coxph(Surv(time, cen)~ LymphN + ResecM + WHO +Diff_Status +
                  Diabetic + LocalInv + PostOpCA199 + MaxTumSiz,
                data = espac3)

# backward selection
cm_step <- list()
kVal <- 2:5
for(k in kVal){
  cm_step[[paste0(k)]] <- stepAIC(cmFull, direction = 'backward', trace = F, k=k) 
}

AIC(cm_step$`2`)
AIC(cm_step$`3`)
AIC(cm_step$`4`)
AIC(cm_step$`5`)
anova(cm_step$`2`,cm_step$`3`, cm_step$`4`, cm_step$`5`)

cm_step$`2`$formula
summary(cm_step$`2`)
# fit flexible parametric model with splines
flsm <- flexsurvspline(Surv(time, cen) ~ LymphN + ResecM + 
                         Diff_Status + PostOpCA199,
                       data = espac3, k = 5) #k=5 is best

flsm
save(flsm,file = "Output/Models/flsm.R")





setwd("/Users/richardjackson/Documents/GitHub/pscRepository/Models/PDAC/Gem_model")

load("CFM.Rds")
dir()

ggarrange(plotlist=CFM$datavis)

