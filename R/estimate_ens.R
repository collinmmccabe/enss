#' estimate_ens
#'
#' estimate_ens takes the outbreak time for a given network and compares this to the
#'   relationship between group size and outbreak time on maximally complete networks to
#'   predict effective network size
#'
#' @param x the number of days that a disease took to break out through the observed network
#' @param predict_model model output from either 'predict_SI_max' or 'predict_SIR_max'
#'
#' @return integer predicted effective network size
#' @export
#'
#' @examples
estimate_ens <- function(x, predict_model) {

  y = round(exp(predict_model$regression.results$Intercept[4] + predict_model$regression.results$Slope[4] * x))

  return(y)
}
