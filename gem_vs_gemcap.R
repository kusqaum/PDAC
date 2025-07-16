library(psc)
library(Cairo)
library(ggplot2)
# compare gemcap data against gem
load("Data/espac4gemcap.R")
load("Output/Models/cfm.Rds")

summary(cfm)

comp <-pscfit(CFM = cfm,DC = espac4_gemcap)

CairoPNG("Output/Images/e3gem_vs_e4gemcap.png",
         width = 600, height = 600, bg = "transparent")
plot(comp) + 
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
summary(comp)
?pscfit()

