#' estimate_backtrans_ens
#'
#' estimate_backtrans_ens takes the outbreak time for a given network and compares
#'   this to the relationship between group size and outbreak time on maximally
#'   complete network to predict effective network size, with a correction for
#'   back-transforming from log-scale.
#'
#' @param x the number of days that a disease took to break out through the observed network
#' @param predict_model model output from either 'predict_SI_max' or 'predict_SIR_max'
#' @param sim_output the output from running 'sim_SI' or 'sim_SIR'
#'
#' @return integer predicted effective network size
#' @export
#'
#' @examples
estimate_backtrans_ens <- function(x, predict_output, sim_output) {

  mod2resid <- rep(NA, nrow(sim_output))
  for (i in 1:nrow(sim_output)) {

    mod2resid[i] <- ((predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * sim_output$days[i]) - log(sim_output$n[i]))^2
  }

  y = round(exp(predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * x + (mean(mod2resid) / 2)))

  return(y)
}
