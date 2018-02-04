estimate_backtrans_ens <- function(x, predict_output, sim_output) {

  mod2resid = rep(NA, nrow(sim_output))
  for (i in 1:nrow(sim_output)) {

    mod2resid[i] <- ((predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * sim_output$days[i]) - log(sim_output$n[i]))^2
  }

  y = round(exp(predict_output$regression.results$Intercept[4] + predict_output$regression.results$Slope[4] * x + mean(mod2resid)/2))

  return(y)
}
