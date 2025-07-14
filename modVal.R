#

library(flexsurv)
library(survival)
library(survminer)
library(cowplot)
library(psc)
library(Cairo)

mod <- readRDS("Output/Models/flsm.rds")
espac3 <- readRDS("Data/espac3clean.rds")

mod_coef <- mod$coefficients
means <- mod$datameans
knot <- mod$knots

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

head(mod$dfns)
eta <- rowSums(basis(knot, log(espac3$stime)) * gam)

espac3_c$lp_eta <- eta+espac3_c$lp
espac3_c$pred <- predict(mod, type = 'lp')

range(espac3_c$pred$.pred_link)




#### cut lp into rgs ####
espac3_c$rg <- cut(espac3_c$lp_eta, breaks = 4,
                   labels = c("1","2","3","4"))

lp_q <- quantile(espac3_c$pred$.pred_link, c(0.25,0.5,0.75))
espac3_c$rg <- cut(espac3_c$pred$.pred_link, breaks = c(-Inf, lp_q, Inf), 
                  labels = c("Risk Group 1","Risk Group 2","Risk Group 3","Risk Group 4"))

# espac3_c$rg_2 <- cut(espac3_c$lp, breaks = 4)

espac3_c$stime <- espac3$stime
espac3_c$cen <- espac3$OS_cen

sfRG <- survfit(Surv(stime,cen)~rg, data = espac3_c)

CairoPNG("Output/Images/e3_gem_discrim_ka.png", 
    width = 600, height = 600, bg = "transparent")
ggsurvplot(sfRG, data=espac3_c,
           palette = c("pink2","purple","cyan3", "dodgerblue3"),
           xlim = c(0,70),
           legend = c(0.65,0.85),
           legend.title = element_blank(), 
           #ggtheme = theme(panel.grid.major = element_line(color = "grey", linetype = "dashed")),
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
      legend.text = element_text(colour="white"))
  
dev.off()

c_slope <- coxph(mod$data$m[,1] ~ espac3_c$pred$.pred_link)
c_slope$concordance[6] # 0.6644871  
c_slope_se <- c_slope$concordance[7]
concordance(c_slope)
#calculate somer's d
(34201     -17246  )/(34201   + 17246   + 92  +   19 + 0  ) #0.3289742 
#or 
2*(c_slope$concordance[6]-0.5)
# 0.3289742 


cm <- coxph(mod$data$m[,1]~rg, data = espac3_c)
summary(cm)

# create CFM
cfm <- pscCFM(mod, dataSumm = T, dataVis = T)

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





