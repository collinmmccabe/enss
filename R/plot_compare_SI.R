#' plot_compare_SI
#'
#' @param sim_erg_output
#' @param sim_max_output
#' @param n_min
#' @param n
#' @param p
#' @param samp
#'
#' @return
#' @export
#'
#' @examples
plot_compare_SI <- function(sim_erg_output, sim_max_output, n_min, n, p, samp) {

  predict_SI_output <- predict_SI_max(sim_max_output, n_min)

  compare_SI_output <- compare_SI_erg_ens(sim_erg_output, sim_max_output, n_min)

  par(oma = c(0, 0, 0, 0))

  layout(matrix(c(1, 1, 2, 3, 4, 4), 3, 2, byrow = TRUE))

  # --------

  SIlist <- list(sim_max_output[[estimate_backtrans_ens(mean(sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][2,] == 0)]), predict_SI_output, sim_max_output) - n_min + 1]][1,], sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][2,] == 0)])

  par(mar=c(4, 2, 2, 0))

  plotrix::multhist(SIlist, xlab = "Outbreak duration (days)", main="(A) Representative comparison of histograms for outbreak durations")
  # --------

  par(mar=c(4, 5, 2, 1))

  plot(compare_SI_output$exp_mu[2:nrow(compare_SI_output)] ~ compare_SI_output$obs_mu[2:nrow(compare_SI_output)], pch = 20, bty = "l", xlab = "Mean outbreak duration on random network", ylab = "Mean outbreak duration on\neffective network", main = "(B) Mean outbreak duration correlation")

  abline(a = 0, b = 1, col = "red")

  # --------

  matplot(t(data.frame(compare_SI_output$obs_sd[2:nrow(compare_SI_output)], compare_SI_output$exp_sd[2:nrow(compare_SI_output)])), type = "b", pch = 20, col = 1, lty = 1, bty = "l", ylab = "Standard deviations", xaxt = "n", main = "(C) Pairwise comparison of standard deviation")

  axis(1, at = c(1, 2), labels = c("Random", "Effective"))

  # --------

  hist(compare_SI_output$kst_D, xlab = "Kolmogorov-Smirnov D-statistic", main = "(D) Distributions of Kolmogorov-Smirnov D-statistics", col = "gray")
}
