#----------------#
#   Simulation   #
#  on Empirical  #
#    Networks    #
#----------------#

Effective_g_emp <- readRDS("Effective_g_emp")
Effective_el_emp <- readRDS("Effective_el_emp")

intxn_per_day = 3
days = 10000
iters = 1000
beta = 0.1
gamma = 0.1

# cat('\n', 'SI empirical models:', '\n', '\n')
# cl <- makeCluster(no_cores, type="FORK")
# clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
# SI_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_emp[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
# stopCluster(cl)
#
# cat('\n', 'SIR empirical models:', '\n', '\n')
# cl <- makeCluster(no_cores, type="FORK")
# clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
# SIR_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_emp[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
# stopCluster(cl)
#
# saveRDS(SI_emp,file="Effective_SI_emp")
# saveRDS(SIR_emp,file="Effective_SIR_emp")
# rm(SI_emp, SIR_emp)


Effective_g_emp_w <- readRDS("./sims_empirical/Effective_g_emp_w")
Effective_el_emp_w <- readRDS("./sims_empirical/Effective_el_emp_w")

SI_emp_w <- lapply(c(1:length(Effective_el_emp_w)), function(x,b,i,d,it) { print(replicate(it, (sim_SI_w(Effective_el_emp_w[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters)

SIR_emp_w <- lapply(c(1:length(Effective_el_emp_w)), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR_w(Effective_el_emp_w[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters)

# saveRDS(SI_emp_w,file="./sims_empirical/Effective_SI_emp_w")
saveRDS(SIR_emp_w,file="./sims_empirical/Effective_SIR_emp_w")
rm(SI_emp_w, SIR_emp_w)
