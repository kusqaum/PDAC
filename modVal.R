#

library(flexsurv)
library(survival)
library(survminer)
library(cowplot)
library(psc)
library(Cairo)
source("Code/discrimKMPlot.R")
source("Code/summ_cox_html.R")

load("Output/Models/flsm.R")
# espac3 <- readRDS("Data/espac3clean.rds")
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

CairoPNG("Output/Images/e3_gem_discrim_ka.png", 
    width = 600, height = 600, bg = "transparent")
discrimKMPlot(sfRG, data = as.data.frame(cov), time = espac3$stime,
              pal = c("pink2","purple","cyan3", "dodgerblue3"),
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
# using espac 4
# xpred <- predict(flsm, newdata = espac4_gem, type = 'lp')
# xlp <- xpred$.pred_link

e4 <- e4_cov[,1:5]
xlp <- t(coef %*% t(e4))
xsobj <- e4_cov[,6]


xrg <- cut(xlp, breaks = c(-Inf, lp_q, Inf),
            labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))

xfit <- survfit(xsobj~xrg)
#discrim
CairoPNG("Output/Images/e4_gem_discrim_ka.png", 
         width = 600, height = 600, bg = "transparent")
ggsurvplot(xfit, data=espac4_gem,
           palette = c("pink2","purple","cyan3", "dodgerblue3"),
           xlim = c(0,70),
           legend = c(0.65,0.85),
           legend.title = element_blank(), 
           legend.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3", "Risk Group 4"),
           xlab= "Time (months)")$plot+
  geom_hline(yintercept = seq(0,1, by = 0.1), lty = 2, colour="grey") +
  geom_vline(xintercept = seq(0,70, by = 10), lty = 2, colour="grey") + 
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', colour=NA),
        legend.background = element_rect(fill='transparent'), 
        legend.box.background = element_rect(fill='transparent'),
        axis.title.x = element_text(face="italic", colour="white"),
        axis.title.y = element_text(face = "italic", colour="white"),
        legend.text = element_text(colour="white"),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))
dev.off()


#calib
xcm <- coxph(xsobj~xlp)
summary(xcm) # slope 0.544
xcm$concordance # 0.59
2*(xcm$concordance[6]-0.5)

xcmrg <- coxph(xsobj~xrg)
summary(xcmrg)
res <- summary(xcmrg)$conf.int


summ_cox_html(xcmrg)
