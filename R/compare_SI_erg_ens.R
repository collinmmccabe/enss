compare_SI_erg_ens <- function(sim_erg_output, sim_max_output, predict_SI_output) {

  nodes <- which(!sapply(sim_erg_output, is.null) & !is.na(sim_erg_output))

  percents <- which(!sapply(sim_erg_output[[nodes[1]]], is.null) & !is.na(sim_erg_output[[nodes[1]]]))

  samples <- length(sim_erg_output[[nodes[1]]][[percents[1]]])


  SI_erg_output <- data.frame(obs_mu = NA, obs_sd = NA, exp_mu = NA, exp_sd = NA, kst_D = NA, kst_p = NA, n = NA, p = NA, samp = NA)

  for (n in nodes) {

    for (p in percents) {

      for(i in 1:samples) {

        tmp.obs <- sim_erg_output[[n]][[p]][[i]][1,which(sim_erg_output[[n]][[p]][[i]][2,] == 0)]

        # need to fix how max models reference number of nodes
        tmp.exp <- sim_max_output[[estimate_backtrans_ens(mean(tmp.obs), predict_SI_output, sim_max_output)]][1,]

        tmp.kst <- ks.test(jitter(tmp.obs), jitter(tmp.exp))

        tmp <- data.frame(obs_mu = mean(tmp.obs), obs_sd = sd(tmp.obs), exp_mu = mean(tmp.exp), exp_sd = sd(tmp.exp), kst_D = tmp.kst$statistic, kst_p = tmp.kst$p.value, n = n, p = p, samp = i)

        SI_erg_output <- rbind(SI_erg_output, tmp)
      }
    }
  }

  return(SI_erg_output)
}
