#' predict_SI_max
#'
#' @param sim_output
#' @param n_min
#'
#' @return
#' @export
#'
#' @examples
predict_SI_max <- function(sim_output, n_min) {

  SI_max_output <- data.frame(days = NA, n = NA)

  for(i in 1:length(sim_output)) {

    tmp <- data.frame(days = sim_output[[i]][1,which(sim_output[[i]][2,] == 0)], n = n_min + i - 1)

    SI_max_output <- rbind(SI_max_output, tmp)
  }

  SImod2 <- lmodel2(log(n) ~ days, data = SI_max_output, range.y = "relative", range.x = "relative")

  return(SImod2)

}
