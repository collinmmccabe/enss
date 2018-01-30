#####################################
# Effective Network Size Simulation #
#               - - -               #
#   Calculate SI/SIR spread time    #
#   for maximally-complete graphs   #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

# Unweighted Simulations

Effective_el_max <- readRDS("./sims_maximal/max2/Effective_el_max2")

SI_maxsim <- list(NA); SIR_maxsim <- list(NA)

for (b in c(0.01, 0.025, 0.05, 0.075, 0.2)) {

  cat('\n', 'beta = ', b, '\n', '\n')

  cat('\n', 'SI maximal models:', '\n', '\n')
  SI_max <- clust_sim_SI(network_el = Effective_el_max, beta = b, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
  SI_maxsim[b*1000] <- SI_max

  for (g in c(0.05, 0.1, 0.15)) {

    cat('\n', 'gamma = ', g, '\n', '\n')

    cat('\n', 'SIR maximal models:', '\n', '\n')
    SIR_max <- clust_sim_SIR(network_el = Effective_el_max, beta = b, gamma = g, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
    SIR_maxsim[b*1000][g*100] <- SIR_max
  }
}

saveRDS(SI_maxsim, file="./data/Effective_SI_maxsim")
saveRDS(SIR_maxsim, file="./data/Effective_SIR_maxsim")
rm(SI_max, SIR_max, SI_maxsim, SIR_maxsim)

#------------------------------------------------

# Uniform Simulations

  cat('\n', 'Uniform SI maximal models:', '\n', '\n')
  SI_max_unif <- clust_sim_SI_unif(network_el = Effective_el_max, beta = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
  saveRDS(SI_max_unif, file="./data/Effective_SI_max_unif"); rm(SI_max_unif)

  cat('\n', 'Uniform SIR maximal models:', '\n', '\n')
  SIR_max_unif <- clust_sim_SIR_unif(network_el = Effective_el_max, beta = 0.1, gamma = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
  saveRDS(SIR_max_unif, file="./data/Effective_SIR_max_unif"); rm(SIR_max_unif)
