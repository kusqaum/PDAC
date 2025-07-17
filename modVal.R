#

library(flexsurv)
library(survival)
library(survminer)
library(cowplot)
library(psc)
library(Cairo)
source("Code/discrimKMPlot.R")

load("Output/Models/flsm.R")
espac3 <- readRDS("Data/espac3clean.rds")
load("Data/espac4gem.R") # for external validation

mod_coef <- flsm$coefficients
means <- flsm$datameans
knot <- flsm$knots
cov <- model.matrix(flsm)
gam <- flsm$coefficients[grep("gamma", names(flsm$coefficients))]
coef <- flsm$coefficients[-grep('gamma', names(flsm$coefficients))]
covs <- substr(names(coef),1,4)
sobj <- flsm$data$m[,1]

espac3_c <- espac3[,grepl(
  paste0(covs[1],"|",covs[2],"|",covs[3],"|",
         covs[4],"|",covs[5]),colnames(espac3))]


lp <- t(coef %*% t(cov))

espac3_c$pred <- predict(flsm, type = 'lp')

range(espac3_c$pred$.pred_link)


lp_q <- quantile(espac3_c$pred$.pred_link, c(0.15,0.5,0.85))
espac3_c$rg <- cut(espac3_c$pred$.pred_link, breaks = c(-Inf, lp_q, Inf), 
                  labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))

# espac3_c$rg_2 <- cut(espac3_c$lp, breaks = 4)



sfRG <- survfit(sobj~espac3_c$rg)

CairoPNG("Output/Images/e3_gem_discrim_ka.png", 
    width = 600, height = 600, bg = "transparent")
discrimKMPlot(sfRG, data = espac3_c, time = espac3$stime,
              pal = c("pink2","purple","cyan3", "dodgerblue3"),
              xlim = c(0,70), leg.x = 0.65, leg.y = 0.85, 
              leg.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3", "Risk Group 4"), 
              vl = 70)
dev.off()

c_slope <- coxph(flsm$data$m[,1] ~ espac3_c$pred$.pred_link)
c_slope$concordance[6] # 0.6644871  
c_slope_se <- c_slope$concordance[7]
concordance(c_slope)
#calculate somer's d
(34201     -17246  )/(34201   + 17246   + 92  +   19 + 0  ) #0.3289742 
#or 
2*(c_slope$concordance[6]-0.5)
# 0.3289742 


cm <- coxph(flsm$data$m[,1]~rg, data = espac3_c)
summary(cm)

# create CFM
cfm <- pscCFM(flsm, dataSumm = T, dataVis = T)

# get plots

plots_lym <- plot(cfm$datavis$LymphN)
plots_rm <- plot(cfm$datavis$ResecM)#
plots_diff <- plot(cfm$datavis$Diff_Status)
plots_ca19 <- plot(cfm$datavis$PostOpCA199)

CairoPNG("Output/Images/e3_gem_ka.png", width = 400, height = 700,
    bg = "transparent")
plot_grid(plots_lym,
          plots_rm,
          plots_diff,
          plots_ca19, ncol=1)
dev.off()


# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)
datasumm <- cfm$datasumm[[1]]

# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)

dev.off()

save(cfm, file = "Output/Models/cfm.Rds")


### code for external validation...

expred <- predict(flsm, newdata = espac4_gem, type = 'lp')
exlp <- expred$.pred_link

xsobj <- Surv(time = espac4_gem$time, event = espac4_gem$cen)
xquant <- quantile(exlp, c(0.15,0.50,0.85))

exrg <- cut(exlp, breaks = c(-Inf, xquant, Inf),
            labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))

xfit <- survfit(xsobj~exrg)
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
        axis.line.y = element_line(colour = "white"))
dev.off()


#calib
xcm <- coxph(xsobj~exlp)
summary(xcm) # slope 0.544
xcm$concordance # 0.59

xcmrg <- coxph(xsobj~exrg)
summary(xcmrg)
xcmrg$coefficients
