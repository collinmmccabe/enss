#' estimate_backtrans_ens
#'
#' estimate_backtrans_ens takes the outbreak time for a given network and compares
#'   this to the relationship between group size and outbreak time on maximally
#'   complete network to predict effective network size, with a correction for
#'   back-transforming from log-scale.
#'
#' @param x the number of days that a disease took to break out through the observed network
#' @param predict_model model output from either 'predict_SI_max' or 'predict_SIR_max'
#' @param sim_max_output the output from running 'sim_SI' or 'sim_SIR' on maximally complete
#'   graphs
#'
#' @return integer predicted effective network size
#' @export
#'
#' @examples
estimate_backtrans_ens <- function(x, predict_output, sim_max_output) {

  mod2resid <- rep(NA, nrow(sim_max_output))
  for (i in 1:nrow(sim_max_output)) {

    mod2resid[i] <- ((predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * sim_max_output$days[i]) - log(sim_max_output$n[i]))^2
  }

  y = round(exp(predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * x + (mean(mod2resid) / 2)))

  return(y)
}
