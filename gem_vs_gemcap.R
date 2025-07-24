library(psc)
library(Cairo)
library(ggplot2)
library(survival)
# compare gemcap data against gem
load("Data/espac4gemcap.R")
load("Output/Models/cfm.Rds")

summary(cfm)

pscfit <-pscfit(CFM = cfm,DC = espac4_gemcap)

CairoPNG("Output/Images/e3gem_vs_e4gemcap.png",
         width = 600, height = 600, bg = "transparent")
plot(pscfit) + 
  theme(panel.background = element_rect(fill='transparent'), 
        plot.background = element_rect(fill='transparent', colour=NA),
        legend.background = element_rect(fill='transparent'), 
        legend.box.background = element_rect(fill='transparent'),
        axis.title.x = element_text(face="italic", colour="white"),
        axis.title.y = element_text(face = "italic", colour="white"),
        legend.text = element_text(colour="white"),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))
dev.off()
summary(pscfit)
post <- pscfit$posterior


#### sub-group analysis ####
espac4_R1 <- espac4_gemcap[which(espac4_gemcap$ResecM==1),]
espac4_R0 <- espac4_gemcap[which(espac4_gemcap$ResecM==0),]
espac4_L1 <- espac4_gemcap[which(espac4_gemcap$LymphN==1),]
espac4_L0 <- espac4_gemcap[which(espac4_gemcap$LymphN==0),]
espac4_Diff0 <- espac4_gemcap[which(espac4_gemcap$Diff_Status==0),]
espac4_Diff1 <- espac4_gemcap[which(espac4_gemcap$Diff_Status==1),]
espac4_Diff2 <- espac4_gemcap[which(espac4_gemcap$Diff_Status==2),]
espac4_CA19low <- espac4_gemcap[which(espac4_gemcap$PostOpCA199 < 20),]
#espac4_CA19high <- espac4_gemcap[which(espac4_gemcap$PostOpCA199 >),]

pscR1 <- pscfit(cfm, espac4_R1)
pscR0 <- pscfit(cfm, espac4_R0)
pscL1 <- pscfit(cfm, espac4_L1)
pscL0 <- pscfit(cfm, espac4_L0)
pscDiff0 <- pscfit(cfm, espac4_Diff0)
pscDiff1 <- pscfit(cfm, espac4_Diff1)
pscDiff2 <- pscfit(cfm, espac4_Diff2)
# psc

# r0 <- coefficients(pscR0)
# temp <- as.data.frame(coefficients(pscR0))
# b <- paste(round(r0[1,1],2), " (",round(r0[1,2],2), ",", round(r0[1,3],2),")", sep = "")
# dic <- paste(round(r0[2,1],2), " (",round(r0[2,2],2), ",", round(r0[2,3],2),")", sep = "")
# med <- rbind(b, dic); rownames(med) <- c("beta", "DIC"); colnames(med)<- c("median (2.5%, 7.5%")
# kable(med, "html")
# summPSCfit(pscR1)
# coef(pscR1
#      )
