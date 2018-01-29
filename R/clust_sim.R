clust_sim_SI <- function(network_el, beta, intxn_per_day, days, iters, free_threads = 1) {

  network_el.tmp <- network_el

  if (.Platform$OS.type == "unix") {
    cl.type <- "FORK"
  } else {
    cl.type <- "PSOCK"
  }

  no_cores <- detectCores() - free_threads

  cl <- makeCluster(no_cores, type = cl.type)

  clusterExport(cl = cl, varlist = c("network_el.tmp", "sim_SI", "beta", "intxn_per_day", "days", "iters"))

  SI_max <- pblapply(c(1:length(network_el.tmp)), function(x,b,i,d,it) { print(replicate(it, (sim_SI(network_el.tmp[[x]],b,i,d)))) }, b = beta, i = intxn_per_day, d = days, it = iters, cl = cl)

  return(SI_max)

  stopCluster(cl)

}
