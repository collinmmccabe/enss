#' estimate_ens
#'
#' @param x
#' @param predict_model
#'
#' @return
#' @export
#'
#' @examples
estimate_ens <- function(x, predict_model) {

  y = round(exp(predict_model$regression.results$Intercept[4] + predict_model$regression.results$Slope[4] * x))

  return(y)
}
