library(psc)
library(ragg)
library(ggplot2)
library(survival)
library(forestplot)
# compare gemcap data against gem
load("Data/espac4gemcap.R")
load("Output/Models/cfm.Rds")
source("Code/discrimKMplot.R")

summary(cfm)

pscfit <-pscfit(CFM = cfm,DC = espac4_gemcap)

agg_png("Output/Images/e3gem_vs_e4gemcap.png",res = 110,
         width = 600, height = 600, bg = "transparent")
plot(pscfit) + 
  theme_mecPortal()
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
espac4_CA19low <- espac4_gemcap[which(espac4_gemcap$PostOpCA199 < 2.5),]
espac4_CA19high <- espac4_gemcap[which(espac4_gemcap$PostOpCA199 >=2.5),]

pscR1 <- pscfit(cfm, espac4_R1)
pscR0 <- pscfit(cfm, espac4_R0)
pscL1 <- pscfit(cfm, espac4_L1)
pscL0 <- pscfit(cfm, espac4_L0)
pscDiff0 <- pscfit(cfm, espac4_Diff0)
pscDiff1 <- pscfit(cfm, espac4_Diff1)
pscDiff2 <- pscfit(cfm, espac4_Diff2)
pscCA1 <- pscfit(cfm, espac4_CA19low)
pscCA2 <- pscfit(cfm,espac4_CA19high)
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
#sub-group anal can be also done like:
# idRM1 <- which(espac4_gemcap$ResecM==1)  then specify the id in the pscfit: psc <- pscfit(cfm,espac4_gemcap, id=idRM1) etc etc


coef1 <- coef(pscR1)[1,]

#### forest plot ####
pscAll <- data.frame(rbind(coef(pscR0)[1,],
                coef(pscR1)[1,],
                coef(pscL0)[1,],
                coef(pscL1)[1,],
                coef(pscDiff0)[1,],
                coef(pscDiff1)[1,],
                coef(pscDiff2)[1,],
                coef(pscCA1)[1,],
                coef(pscCA2)[1,]))
names(pscAll) <- c("median","lower","upper","p0","p1")
med <- c(pscAll[,1])
label <- c("Resection Margin: Negative",
           "Resection Margin: Positive",
           "Lymph Nodes: Negative",
           "Lymph Nodes: Positive",
           "Tumour Differentiation Status: Poor",
           "Tumour Differentiation Status: Moderate",
           "Tumour Differentiation Status: Well",
           "Log(Post-operative CA19-9): <2.5",
           "Log(Post-operative CA19-9): >=2.5")

agg_png("Output/Images/sub-group_forest.png",res = 180,
        width = 650, height = 350,bg="transparent")
forestplot(mean =pscAll$median,
           lower=pscAll$lower,
           upper=pscAll$upper,
           labeltext=label,
           vertices=T,
           lwd.ci=1,
           boxsize=0.2,
           txt_gp = fpTxtGp(cex=0.45,ticks=gpar(cex=0.45)))|>
  fp_set_style(box = "lightsteelblue",line = "darkblue",
             summary = "lightsteelblue")#|>

dev.off()

