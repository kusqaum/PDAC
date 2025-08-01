source("Code/discrimKMPlot.R")

validateFLSM <- function(flsm, q1 = "auto",q2="auto",q3="auto",...){
  
  library(flexsurv)
  library(survival)
  library(survminer)
  library(knitr) 
  
  # if(q1=="auto" & q2=="auto" &q3=="auto") q1<-0.15;q2<-0.5;q3<-0.85;quantiles<-c(0.15,0.5,0.85)
  # if(q1!="auto"|q2!="auto"|q3!="auto") quantiles<- c(q1,q2,q3)
  if(q1=="auto") q1 <- 0.15
  if(q2=="auto") q2 <- 0.5
  if(q3=="auto") q3 <- 0.85
  
  quantiles <- c()
  if(q1!="auto") append(quantiles, q1)
  if(q2!="auto") append(quantiles, q2)
  if(q3!="auto") append(quantiles, q3)
  quantiles <- c(q1,q2,q3)
  # print(quantiles)
  gam <- flsm$coefficients[grep("gamma", names(flsm$coefficients))]
  coef <- flsm$coefficients[-grep('gamma', names(flsm$coefficients))]
  mat <- model.matrix(flsm)
  sobj <- flsm$data$m[,1]
  
  summ_cox_html <- function(cm){
    res <- summary(cm)$conf.int
    resest <- summary(cm)$coefficients
    xhr <- paste(round(res[,1],2), " (", round(res[,3],2), "-", round(res[,4],2), ")", sep = "")
    xest  <- paste(round(cm$coefficients, 2), " (", round(resest[,3],2), ")", sep = "") #;names(xest) <- NULL
    cm_kab <- cbind(xest,xhr)
    colnames(cm_kab) <- c("Est (se)", "HR (95% CI)")
    k<- kable(cm_kab, "html")
    return(k)
  }
  
  # discrim
  
  lp <- (coef %*% t(mat))
  lpq <- quantile(lp, quantiles)
  lp_rg <- cut(lp, c(-Inf, lpq, Inf), labels = c("Risk group 1",
                                                 "Risk group 2",
                                                 "Risk group 3",
                                                 "Risk group 4"))
  data <- flsm$data$m[,-c(1)]
  data$sobj <- flsm$data$m[,1]
  data$lp_rg <- lp_rg
  time <-sobj[,1]
  fit <- survfit(data$sobj ~ lp_rg)
  # plot <- ggsurvplot(fit,data=data)
  plot <- discrimKMPlot(fit, data, time,
                        pal = c("pink2","purple","cyan3", "dodgerblue3"),
                        xlim = c(0,70), leg.x = 0.85, leg.y = 0.75,
                        leg.labs = c("Risk Group 1", "Risk Group 2", "Risk Group 3", "Risk Group 4"),
                        vl = 70)
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
  cm_sl <- coxph(sobj~t(lp))
  slope <- cm_sl$coefficients
  slope_se <- round((summary(cm_sl)$coefficients)[3],3)
  
  return(list(calib = list(concordance = concordance,
                           se = concordance_se,
                           slope = slope,
                           slope_se = slope_se,
                           somerd = som),
              discrim = list(rgHRhtml = rg_kable,
                             res = hrs,
                             plot = plot),
              data = list(fit = fit,
                          cov = mat, 
                          
                          lpq = lpq),
              q=quantiles))
}

# load("M:/Documents/Projects/Fellowship/modelDevelop/PDAC/Output/Models/flsm.R")
# validRes <- validateFLSM(flsm)
# ggsurvplot(fit = test$data$fit, data = as.data.frame(test$data$cov))
# plot(validRes$data$fit, col = c(1,2,3,4))
