#' compare_SI_erg_ens
#'
#' compare_SI_erg_ens calculates multiple metrics of comparison between maximally
#'   complete networks and Erdos-Renyi random graphs for clustered SI simulations
#'
#' @param sim_erg_output output of SI simulations over all Erdos-Renyi random graphs,
#'   preferably in the format of a nested list, where the first level corresponds to
#'   the number of nodes, the second level to percent completion of ties, and the final
#'   to the sample number
#' @param sim_max_output output of SI simulations over all maximally complete graphs
#' @param n_min minimum number of nodes in the maximally complete graphs
#'
#' @return a data frame where each row corresponds to a row in sim_erg_output, and the
#'   columns represent (in this order): mean observed (Erdos-Renyi) value, standard deviation of the
#'   observed values, mean expected (effective network size prediction) value, standard
#'   deviation of expected values, Kolmogorov-Smirnov (K-S) test D-statistic, K-S test
#'   p-value, number of nodes in observed network, percent of ties present in observed
#'   network, and the sample number of the observed network (within its node counts and
#'   percent completion)
#' @export
#'
#' @examples
compare_SI_erg_ens <- function(sim_erg_output, sim_max_output, n_min) {

  nodes <- which(!sapply(sim_erg_output, is.null) & !is.na(sim_erg_output))

  percents <- which(!sapply(sim_erg_output[[nodes[1]]], is.null) & !is.na(sim_erg_output[[nodes[1]]]))

  samples <- length(sim_erg_output[[nodes[1]]][[percents[1]]])

  predict_SI_output <- predict_SI_max(sim_max_output, n_min)

  SI_erg_output <- data.frame(obs_mu = NA, obs_sd = NA, exp_mu = NA, exp_sd = NA, kst_D = NA, kst_p = NA, n = NA, p = NA, samp = NA)

  for (n in nodes) {

    for (p in percents) {

      for(i in 1:samples) {

        tmp.obs <- sim_erg_output[[n]][[p]][[i]][1,which(sim_erg_output[[n]][[p]][[i]][2,] == 0)]

        # need to fix how max models reference number of nodes - this works, but it is messy
        tmp.exp <- sim_max_output[[estimate_backtrans_ens(mean(tmp.obs), predict_SI_output, sim_max_output) - n_min + 1]][1,]

        tmp.kst <- ks.test(jitter(tmp.obs), jitter(tmp.exp))

        tmp <- data.frame(obs_mu = mean(tmp.obs), obs_sd = sd(tmp.obs), exp_mu = mean(tmp.exp), exp_sd = sd(tmp.exp), kst_D = tmp.kst$statistic, kst_p = tmp.kst$p.value, n = n, p = p, samp = i)

        SI_erg_output <- rbind(SI_erg_output, tmp)
      }
    }
  }

  return(SI_erg_output)
}
