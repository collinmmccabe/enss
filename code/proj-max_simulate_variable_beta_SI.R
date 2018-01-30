#####################################
# Effective Network Size Simulation #
#               - - -               #
#    Calculate SI spread time for   #
#   maximally-complete graphs with  #
#      variable values for beta     #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

Effective_el_max <- readRDS("./sims_maximal/max2/Effective_el_max2")

SI_max_varb <- list(NA)

for (b in c(0.025, 0.05, 0.075, 0.125, 0.15, 0.175, 0.2)) {

  cat('\n', 'beta = ', b, '\n', '\n')

  cat('\n', 'SI maximal models:', '\n', '\n')
  SI_max_varb.tmp <- clust_sim_SI(network_el = Effective_el_max, beta = b, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
  SI_max_varb[b*1000] <- SI_max_varb.tmp
}

saveRDS(SI_maxsim, file="./data/Effective_SI_max_varb"); rm(SI_max_varb, SI_max_varb.tmp)
