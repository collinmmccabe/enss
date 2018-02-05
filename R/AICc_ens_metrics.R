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

    AICc_vector[i] <- AICc(allModelsResults[[i]]) - (2 * sum(regMat[i,] == TRUE))
  }

  return(allModelsResults[[which.min(AICc_vector)]])
}
