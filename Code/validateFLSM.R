validateFLSM <- function(flsm){
  library(flexsurv)
  library(survival)
  library(survminer)
  library(knitr)
  
  gam <- flsm$coefficients[grep("gamma", names(flsm$coefficients))]
  coef <- flsm$coefficients[-grep('gamma', names(flsm$coefficients))]
  mat <- model.matrix(flsm)
  sobj <- flsm$data$m[,1]
  
  summ_cox_html <- function(cm){
    res <- summary(cm)$conf.int
    xhr <- paste(round(res[,1],2), " (", round(res[,3],2), "-", round(res[,4],2), ")", sep = "")
    xest  <- round(cm$coefficients, 2) #;names(xest) <- NULL
    cm_kab <- cbind(xest,xhr)
    colnames(cm_kab) <- c("Est (se)", "HR (95% CI)")
    k<- kable(cm_kab, "html")
    return(k)
  }
  
  # discrim
  
  lp <- (coef %*% t(mat))
  lpq <- quantile(lp, c(0.15,0.50,0.85))
  lp_rg <- cut(lp, c(-Inf, lpq, Inf), labels = c("Risk group 1",
                                                 "Risk group 2",
                                                 "Risk group 3",
                                                 "Risk group 4"))
  
  
  
  fit <- survfit(sobj ~ lp_rg)
  cm <- coxph(sobj ~ lp_rg)
  # hrs:
  res <- as.data.frame(summary(cm)$conf.int)
  hrs <- res[,-c(2)]
  hrs$est <- coefficients(cm)
  hrs <- hrs[,c(ncol(hrs),1:(ncol(hrs)-1))]
  rg_kable <- summ_cox_html(cm)

  # calib
  # concordance:
  concordance <- cm$concordance[6]
  concordance_se <- cm$concordance[7]
  
  #somers d:
  som <- 2*(concordance-0.5)
  
  # slope 
  cm_sl <- coxph(flsm$data$m[,1]~t(lp))
  
  return(list(calib = list(concordance = concordance,
                           se = concordance_se,
                           somerd = som),
              discrim = list(rgHRhtml = rg_kable,
                             res = hrs),
              data = list(fit = fit,
                          cov = mat, 
                          sobj = sobj,
                          lpq = lpq)))
}

# load("M:/Documents/Projects/Fellowship/modelDevelop/PDAC/Output/Models/flsm.R")
validRes <- validateFLSM(flsm)
# ggsurvplot(fit = test$data$fit, data = as.data.frame(test$data$cov))
plot(test$data$fit, col = c(1,2,3,4))
