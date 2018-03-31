#' predict_SI_max
#'
#' predict_SI_max generates a model II regression prediction for the relationship between
#'   outbreak time and maximally complete network size for SI transmission simulations
#'
#' @param sim_max_output simulation output from any of the sim_SI functions (including
#'   sim_SI_unif and sim_SI_w) in enss using maximally complete graphs
#' @param n_min the minimum network size in the set of maximally complete graphs used
#'   to generate sim_max_output in the first argument
#'
#' @return lmodel2-type regression model object
#' @export
#'
#' @examples
predict_SI_max <- function(sim_max_output, n_min) {

  SI_max_output <- data.frame(days = NA, n = NA)

  for(i in 1:length(sim_max_output)) {

    tmp <- data.frame(days = sim_max_output[[i]][1,which(sim_max_output[[i]][2,] == 0)], n = n_min + i - 1)

    SI_max_output <- rbind(SI_max_output, tmp)
  }

  SImod2 <- lmodel2::lmodel2(log(n) ~ days, data = SI_max_output, range.y = "relative", range.x = "relative")

  return(SImod2)

}
