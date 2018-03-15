#' predict_SIR_max
#'
#' predict_SIR_max generates a model II regression prediction for the relationship between
#'   outbreak time and maximally complete network size for SIR transmission simulations
#'
#' @param sim_output simulation output from any of the sim_SIR functions (including
#'   sim_SIR_unif and sim_SIR_w) in enss using maximally complete graphs
#' @param n_min the minimum network size in the set of maximally complete graphs used
#'   to generate sim_output in the first argument
#'
#' @return lmodel2-type regression model object
#' @export
#'
#' @examples
predict_SIR_max <- function(sim_output, n_min) {

  SIR_max_output <- data.frame(days = NA, peak = NA, n = NA)

  for(i in 1:length(sim_output)) {

    tmp <- data.frame(days = sim_output[[i]][1,which(sim_output[[i]][4,] == i)], peak = sim_output[[i]][5,which(sim_output[[i]][4,] == i)], n = n_min + i - 1)

    SIR_max_output <- rbind(SIR_max_output, tmp)
  }

  SIRmod2 <- lmodel2::lmodel2(log(n) ~ days, data = SIR_max_output, range.y = "relative", range.x = "relative")

  return(SIRmod2)
}
