#' @exportdev

mlr_bestfit_clickstream <- function(maxOrder, data){
  maxOrder <- maxOrder
  result <- data.frame()
  for (k in 1:maxOrder) {
    mc <- fitMarkovChain(clickstreamList = data, order = k)
    result <- rbind(result, c(k,summary(mc)$aic, summary(mc)$bic))
  }
  names(result) <- c("Order", "AIC", "BIC")
  print(result)
}