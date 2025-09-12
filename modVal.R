#

library(flexsurv)
library(survival)
library(survminer)
library(cowplot)
library(psc)
library(ragg)
source("Code/discrimKMPlot.R")
source("Code/summ_cox_html.R")

load("Output/Models/flsm.R")
espac3 <- readRDS("Data/espac3clean.rds")
load("Data/espac4gem.R") # for external validation
load("Data/e4_cov.R")




cov <- model.matrix(flsm)
gam <- flsm$coefficients[grep("gamma", names(flsm$coefficients))]
coef <- flsm$coefficients[-grep('gamma', names(flsm$coefficients))]

sobj <- flsm$data$m[,1]


#### internal validation ####
lp <- t(coef %*% t(cov))

lp_q <- quantile(lp, c(0.15,0.5,0.85))
rg <- cut(lp, breaks = c(-Inf, lp_q, Inf), 
          labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))




sfRG <- survfit(sobj~rg)

agg_png("Output/Images/e3_gem_discrim_ka.png", 
    width = 600, height = 600, bg = "transparent",res=110)
discrimKMPlot(sfRG, data = as.data.frame(cov),lw = 1.5, time = espac3$stime,
              pal = c("#feebe2","#fbb4b9","#f768a1","#ae017e"),
              xlim = c(0,70), leg.x = 0.65, leg.y = 0.85, 
              leg.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3", "Risk Group 4"), 
              vl = 70)
dev.off()


# concordance
cm <- coxph(flsm$data$m[,1]~rg)
summary(cm)$concordance
c_statitic <- cm$concordance[6]
c_statitic_se <- cm$concordance[7]
2*(cm$concordance[6]-0.5)
 
# slope
c_slope <- coxph(flsm$data$m[,1] ~ lp)


####  external validation... ####
# using espac 4 gem treated patients
# xpred <- predict(flsm, newdata = espac4_gem, type = 'lp')
# xlp <- xpred$.pred_link

e4 <- e4_cov[,1:ncol(e4_cov)-1]
xlp <- t(coef %*% t(e4))
xsobj <- e4_cov[,ncol(e4_cov)]


xrg <- cut(xlp, breaks = c(-Inf, lp_q, Inf),
            labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))

xfit <- survfit(xsobj~xrg)
#discrim
agg_png("Output/Images/e4_gem_discrim_ka.png", 
         width = 600, height = 600, bg = "transparent")
ggsurvplot(xfit, data=espac4_gem,
           palette = ("#feebe2","#fbb4b9","#f768a1","#ae017e"),
           xlim = c(0,70),
           legend = c(0.65,0.85),
           legend.title = element_blank(), 
           legend.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3", "Risk Group 4"),
           xlab= "Time (months)")$plot+
  geom_hline(yintercept = seq(0,1, by = 0.1), lty = 2, colour="grey") +
  geom_vline(xintercept = seq(0,70, by = 10), lty = 2, colour="grey") + 
  theme_mecPortal()
dev.off()


#calib
xcm <- coxph(xsobj~xlp) # slope
summary(xcm) # slope 0.544
xcm$concordance # 0.59
2*(xcm$concordance[6]-0.5)

xcmrg <- coxph(xsobj~xrg)
summary(xcmrg)
res <- summary(xcmrg)$conf.int


summ_cox_html(xcmrg)
