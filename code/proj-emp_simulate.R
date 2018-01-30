#####################################
# Effective Network Size Simulation #
#               - - -               #
#   Calculate SI/SIR spread time    #
#       for empirical graphs        #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

# Unweighted Simulations

Effective_el_emp <- readRDS("./sims_empirical/Effective_el_emp")

cat('\n', 'SI empirical models:', '\n', '\n')
SI_emp <- clust_sim_SI(network_el = Effective_el_emp, beta = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
saveRDS(SI_emp, file="./data/Effective_SI_emp"); rm(SI_emp)

cat('\n', 'SIR empirical models:', '\n', '\n')
SIR_emp <- clust_sim_SIR(network_el = Effective_el_emp, beta = 0.1, gamma = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
saveRDS(SIR_emp, file="./data/Effective_SIR_emp"); rm(SIR_emp)

#------------------------------------------------

# Weighted Simulations

Effective_el_emp_w <- readRDS("./sims_empirical/Effective_el_emp_w")

cat('\n', 'Weighted SI empirical models:', '\n', '\n')
SI_emp_w <- clust_sim_SI_w(network_el = Effective_el_emp_w, beta = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
saveRDS(SI_emp_w, file="./data/Effective_SI_emp_w"); rm(SI_emp_w)

cat('\n', 'Weighted SIR empirical models:', '\n', '\n')
SIR_emp_w <- clust_sim_SIR_w(network_el = Effective_el_emp_w, beta = 0.1, gamma = 0.1, intxn_per_day = 3, days = 10000, iters = 1000, free_threads = 1)
saveRDS(SIR_emp_w, file="./data/Effective_SIR_emp_w"); rm(SIR_emp_w)
