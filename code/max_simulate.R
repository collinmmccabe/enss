#####################################
# Effective Network Size Simulation #
#               - - -               #
#  Calculate SI/SIS/SIR/STD spread  #
#    time for maximally-complete,   #
#  Erdos-Renyi, & empirical graphs  #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################


#----------------#
#   Simulation   #
#  on Maximally  #
#  Complete Net  #
#----------------#

Effective_g_max <- readRDS("./sims_maximal/max2/Effective_g_max2")
Effective_el_max <- readRDS("./sims_maximal/max2/Effective_el_max2")

intxn_per_day = 3
days = 10000
SIS_days = 200
iters = 1000
nmin=3
nmax=80

SI_maxsim <- list(NA); SIS_maxsim <- list(NA); SIR_maxsim <- list(NA); STD_maxsim <- list(NA)

for (beta in c(0.01, 0.025, 0.05, 0.075, 0.2)) {

  cat('\n', 'beta = ', beta, '\n', '\n')

  cat('\n', 'SI maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  SI_maxsim[beta*1000] <- SI_max
  stopCluster(cl)

  cat('\n', 'STD maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
  STD_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,mm,mf,fm,ff,it) { print(replicate(it, (sim_STD(Effective_el_max[[x]],b,i,d,mm,mf,fm,ff)))) }, b=beta, i=intxn_per_day, d=days, mm=MM, mf=MF, fm=FM, ff=FF, it=iters, cl=cl)
  STD_maxsim[beta*1000] <- STD_max
  stopCluster(cl)

  for (gamma in c(0.05, 0.1, 0.15)) {

    cat('\n', 'gamma = ', gamma, '\n', '\n')

    cat('\n', 'SIS maximal models:', '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
    SIS_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIS(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=SIS_days, it=iters, cl=cl)
    SIS_maxsim[beta*1000][gamma*100] <- SIS_max
    stopCluster(cl)

    cat('\n', 'SIR maximal models:', '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
    SIR_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
    SIR_maxsim[beta*1000][gamma*100] <- SIR_max
    stopCluster(cl)
  }
}

saveRDS(SI_maxsim, file="Effective_SI_maxsim")
saveRDS(STD_maxsim, file="Effective_STD_maxsim")
saveRDS(SIS_maxsim, file="Effective_SIS_maxsim")
saveRDS(SIR_maxsim, file="Effective_SIR_maxsim")
rm(SI_max, SIS_max, STD_max, SIR_max, SI_maxsim, SIS_maxsim, STD_maxsim, SIR_maxsim)

# Effective_g_max <- readRDS("Effective_g_max")
# Effective_el_max <- readRDS("Effective_el_max")
#
# MM = 4
# MF = 1
# FM = 0.5
# FF = 0.1
# intxn_per_day = 3
# days = 10000
# SIS_days = 200
# iters = 1000
#
# nmin=6; nmax=200
#
# SI_maxsim <- list(NA); SIS_maxsim <- list(NA); SIR_maxsim <- list(NA); STD_maxsim <- list(NA)
#
# for (bet in c(0.01, 0.025, 0.05, 0.075, 0.2)) {
#
#   cat('\n', 'beta = ', bet, '\n', '\n')
#
#   beta = bet
#
#   cat('\n', 'SI maximal models:', '\n', '\n')
#   cl <- makeCluster(no_cores, type="FORK")
#   clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
#   SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
#   SI_maxsim[beta*1000] <- SI_max
#   stopCluster(cl)
#
#   cat('\n', 'STD maximal models:', '\n', '\n')
#   cl <- makeCluster(no_cores, type="FORK")
#   clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
#   STD_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,mm,mf,fm,ff,it) { print(replicate(it, (sim_STD(Effective_el_max[[x]],b,i,d,mm,mf,fm,ff)))) }, b=beta, i=intxn_per_day, d=days, mm=MM, mf=MF, fm=FM, ff=FF, it=iters, cl=cl)
#   STD_maxsim[beta*1000] <- STD_max
#   stopCluster(cl)
#
#   for (gamma in c(0.05, 0.1, 0.15)) {
#
#     cat('\n', 'gamma = ', gamma, '\n', '\n')
#
#     cat('\n', 'SIS maximal models:', '\n', '\n')
#     cl <- makeCluster(no_cores, type="FORK")
#     clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
#     SIS_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIS(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=SIS_days, it=iters, cl=cl)
#     SIS_maxsim[beta*1000][gamma*100] <- SIS_max
#     stopCluster(cl)
#
#     cat('\n', 'SIR maximal models:', '\n', '\n')
#     cl <- makeCluster(no_cores, type="FORK")
#     clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
#     SIR_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
#     SIR_maxsim[beta*1000][gamma*100] <- SIR_max
#     stopCluster(cl)
#   }
# }
#
# saveRDS(SI_maxsim, file="Effective_SI_maxsim")
# saveRDS(STD_maxsim, file="Effective_STD_maxsim")
# saveRDS(SIS_maxsim, file="Effective_SIS_maxsim")
# saveRDS(SIR_maxsim, file="Effective_SIR_maxsim")
# rm(SI_max, SIS_max, STD_max, SIR_max, SI_maxsim, SIS_maxsim, STD_maxsim, SIR_maxsim)

beta = 0.1
gamma = 0.1
SI_max_unif <- list(NA); SIR_max_unif <- list(NA)

  cat('\n', 'SI maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI_unif", "beta", "days", "iters"))
  SI_max_unif <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,d,it) { print(replicate(it, (sim_SI_unif(Effective_el_max[[x]],b,d)))) }, b=beta, d=days, it=iters, cl=cl)
  stopCluster(cl)

  cat('\n', 'SIR maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIR_unif", "beta", "gamma", "days", "iters"))
  SIR_max_unif <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,d,it) { print(replicate(it, (sim_SIR_unif(Effective_el_max[[x]],b,g,d)))) }, b=beta, g=gamma, d=days, it=iters, cl=cl)
  stopCluster(cl)

saveRDS(SI_max_unif, file="Effective_SI_max_unif")
saveRDS(SIR_max_unif, file="Effective_SIR_max_unif")
rm(SI_max_unif, SIR_max_unif)
