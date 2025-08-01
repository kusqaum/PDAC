summ_cox_html <- function(cm){
  library(knitr)
  res <- summary(cm)$conf.int
  resest <- summary(cm)$coefficients
  xhr <- paste(round(res[,1],2), " (", round(res[,3],2), "-", round(res[,4],2), ")", sep = "")
  xest  <- paste(round(cm$coefficients, 2), " (", round(resest[,3],2), ")", sep = "") #;names(xest) <- NULL
  cm_kab <- cbind(xest,xhr)
  colnames(cm_kab) <- c("Est (se)", "HR (95% CI)")
  kable(cm_kab, "html")
}
