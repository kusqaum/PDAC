library(psc)
library(survival)
library(ggpubr)
load("Output/Models/flsm.R")


# create CFM
cfm <- pscCFM(flsm, dataSumm = T, dataVis = T)

# get plots ready
plots <- list()
for(p in 1:length(cfm$datavis)){
  plots[[p]] <- plot(cfm$datavis[[p]])
}

dev.off()

CairoPNG("Output/Images/e3_gem_ka.png", width = 400, height = 700,
         bg = "transparent")

ggarrange(plotlist = plots, ncol = 1)
dev.off()


# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)
datasumm <- cfm$datasumm$summ_Table

# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)

dev.off()

save(cfm, file = "Output/Models/cfm.Rds")
