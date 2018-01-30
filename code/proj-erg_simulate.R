#####################################
# Effective Network Size Simulation #
#               - - -               #
#   Calculate SI/SIR spread time    #
#       for Erdos-Renyi graphs      #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

Effective_el_erg <- readRDS("./sims_ERG/Effective_el_erg")

SI_erg <- list(NA); SIR_erg <- list(NA)
nodes <- c(10, 30, 50); percents <- c(0.15, 0.25, 0.35)

for (n in nodes) {

  SI_erg.tmp <- list(NA)
  SIR_erg.tmp <- list(NA)

  for (p in (percents*100)) {

    Effective_el_erg.tmp <- Effective_el_erg[[n]][[p]]

    cat('\n', 'SI ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    SI_erg.tmp[[p]] <- clust_sim_SI(network_el = Effective_el_erg.tmp, beta = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)

    cat('\n', 'SIR ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    SIR_erg.tmp[[p]] <- clust_sim_SIR(network_el = Effective_el_erg.tmp, beta = 0.1, gamma = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
  }

  SI_erg[[n]] <- SI_erg.tmp
  SIR_erg[[n]] <- SIR_erg.tmp
}

saveRDS(SI_erg, file="./data/Effective_SI_erg")
saveRDS(SIR_erg, file="./data/Effective_SIR_erg")
rm(SI_erg, SI_erg.tmp, SIR_erg, SIR_erg.tmp, n, nodes, p, percents, Effective_el_erg.tmp)
