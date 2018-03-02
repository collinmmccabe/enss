#' AICc_ens_metrics
#'
#' @param ens
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
AICc_ens_metrics <- function(ens, metrics) {

  require(MuMIn)

  # Code adapted from ryouready.wordpress.com/2009/02/06/r-calculating-all-possible-linear-regression-models-for-a-given-set-of-predictors/

  regressors <- colnames(metrics[-1])

  data <- cbind(metrics, ens = ens)

  regMat <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE), c(TRUE,FALSE))
  regMat <- regMat[-(dim(regMat)[1]),]

  names(regMat) <- regressors

  allModelsList <- apply(regMat, 1, function(x) as.formula(paste(c("ens ~ ", paste(regressors[x], collapse=" + ")), collapse="")))

  allModelsResults <- lapply(allModelsList, function(x) lm(x, data=data))

  AICc_vector <- c(NA)
  for(i in 1:length(allModelsResults)) {

    AICc_vector[i] <- AICc(allModelsResults[[i]])
  }

  AICc_returns <- list(NA); k <- 1
  for(j in which(AICc_vector < AICc_vector[which.min(AICc_vector)] + 2)) {

    AICc_returns[[k]] <- allModelsResults[[j]]
    k <- k + 1
  }

  return(AICc_returns)
}
