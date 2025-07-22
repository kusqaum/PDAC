library(psc)
library(survival)
load("Output/Models/flsm.R")


# create CFM
cfm <- pscCFM(flsm, dataSumm = T, dataVis = T)

# get plots

# plots_lym <- plot(cfm$datavis$LymphN)
# plots_rm <- plot(cfm$datavis$ResecM)#
# plots_diff <- plot(cfm$datavis$Diff_Status)
# plots_ca19 <- plot(cfm$datavis$PostOpCA199)

plots <- list()
for(p in 1:length(cfm$datavis)){
  plots[[p]] <- plot(cfm$datavis[[p]])
}


# plot_grid(plots_lym,
#           plots_rm,
#           plots_diff,
#           plots_ca19, ncol=1)
dev.off()
CairoPNG("Output/Images/e3_gem_ka.png", width = 400, height = 700,
         bg = "transparent")

ggarrange(plotlist = plots, ncol = 1)
dev.off()


# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)
datasumm <- cfm$datasumm[[1]]

# png("Output/Images/e3_dataSumm_ka.png", width = 600, height = 600)

dev.off()

save(cfm, file = "Output/Models/cfm.Rds")
