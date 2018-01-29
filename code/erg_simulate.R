#---------------#
#  Simulations  #
#    on ERGs    #
#---------------#

# use Latin hypercube/orthogonal sampling to select a representative subset?

Effective_g_erg <- readRDS("Effective_g_erg")
Effective_el_erg <- readRDS("Effective_el_erg")

SI_erg<-list(NA)
SIR_erg<-list(NA)

for (n in nodes) {

  SI_tmp<-list(NA)
  SIR_tmp<-list(NA)

  for (p in (percents*100)) {

    cat('\n', 'SI ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
    SI_tmp[[p]] <- pblapply(c(1:samps), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_erg[[n]][[p]][[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
    stopCluster(cl)

    cat('\n', 'SIR ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
    SIR_tmp[[p]] <- pblapply(c(1:samps), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_erg[[n]][[p]][[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
    stopCluster(cl)
  }

  SI_erg[[n]]<-SI_tmp
  SIR_erg[[n]]<-SIR_tmp
}

saveRDS(SI_erg,file="Effective_SI_erg")
saveRDS(SIR_erg,file="Effective_SIR_erg")
rm(SI_erg, SIR_erg, n, p)
