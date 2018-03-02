#' plot_compare_SIR
#'
#' @param sim_erg_output
#' @param sim_max_output
#' @param max_nmin
#' @param n
#' @param p
#' @param samp
#'
#' @return
#' @export
#'
#' @examples
plot_compare_SIR <- function(sim_erg_output, sim_max_output, max_nmin, n, p, samp) {

  predict_SIR_output <- predict_SIR_max(sim_max_output, max_nmin)

  compare_SIR_output <- compare_SIR_erg_ens(sim_erg_output, sim_max_output, max_nmin)

  par(oma = c(0, 0, 0, 0))

  layout(matrix(c(1, 1, 2, 3, 4, 4), 3, 2, byrow = TRUE))

  # --------

  SIRlist <- list(sim_max_output[[estimate_backtrans_ens(mean(sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][4,] == n)]), predict_SIR_output, sim_max_output) - n_min + 1]][1,which(sim_max_output[[estimate_backtrans_ens(mean(sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][4,] == n)]), predict_SIR_output, sim_max_output) - n_min + 1]][4,] == estimate_backtrans_ens(mean(sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][4,] == n)]), predict_SIR_output, sim_max_output) - n_min + 1)], sim_erg_output[[n]][[p]][[samp]][1,which(sim_erg_output[[n]][[p]][[samp]][4,] == n)])

  par(mar=c(4, 2, 2, 0))

  multhist(SIRlist, xlab = "Outbreak duration (days)", main="(A) Representative comparison of histograms for outbreak durations")
  # --------

  par(mar=c(4, 5, 2, 1))

  plot(compare_SIR_output$exp_mu[2:nrow(compare_SIR_output)] ~ compare_SIR_output$obs_mu[2:nrow(compare_SIR_output)], pch = 20, bty = "l", xlab = "Mean outbreak duration on random network", ylab = "Mean outbreak duration on\neffective network", main = "(B) Mean outbreak duration correlation")

  abline(a = 0, b = 1, col = "red")

  # --------

  matplot(t(data.frame(compare_SIR_output$obs_sd[2:nrow(compare_SIR_output)], compare_SIR_output$exp_sd[2:nrow(compare_SIR_output)])), type = "b", pch = 20, col = 1, lty = 1, bty = "l", ylab = "Standard deviations", xaxt = "n", main = "(C) Pairwise comparison of standard deviation")

  axis(1, at = c(1, 2), labels = c("Random", "Effective"))

  # --------

  hist(compare_SIR_output$kst_D, xlab = "Kolmogorov-Smirnov D-statistic", main = "(D) Distributions of Kolmogorov-Smirnov D-statistics", col = "gray")
}
