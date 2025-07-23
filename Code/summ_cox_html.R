summ_cox_html <- function(cm){
  library(knitr)
  res <- summary(cm)$conf.int
  xhr <- paste(round(res[,1],2), " (", round(res[,3],2), "-", round(res[,4],2), ")", sep = "")
  xest  <- round(xcmrg$coefficients, 2) #;names(xest) <- NULL
  cm_kab <- cbind(xest,xhr)
  colnames(cm_kab) <- c("Est (se)", "HR (95% CI)")
  kable(cm_kab, "html")
}
