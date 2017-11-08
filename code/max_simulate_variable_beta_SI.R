#####################################
# Effective Network Size Simulation #
#               - - -               #
#  Calculate SI/SIS/SIR/STD spread  #
#    time for maximally-complete,   #
#  Erdos-Renyi, & empirical graphs  #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################


nmin=6; nmax=120

#----------------#
#   Simulation   #
#  on Maximally  #
#  Complete Net  #
#----------------#

Effective_g_max <- readRDS("Effective_g_max2")
Effective_el_max <- readRDS("Effective_el_max2")

intxn_per_day = 3
days = 10000
iters = 1000


beta = 0.025
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max025")
rm(SI_max)


beta = 0.050
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max050")
rm(SI_max)


beta = 0.075
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max075")
rm(SI_max)


beta = 0.125
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max125")
rm(SI_max)


beta = 0.150
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max150")
rm(SI_max)


beta = 0.175
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max175")
rm(SI_max)


beta = 0.200
SI_max <- list(NA)
cat('\n', 'SI maximal model; beta = ', beta, '\n', '\n')

  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max, file="Effective_SI_max200")
rm(SI_max)
