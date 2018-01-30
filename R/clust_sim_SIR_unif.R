clust_sim_SIR_unif <- function(network_el, beta, gamma, days, iters, free_threads = 1) {

  if (.Platform$OS.type == "unix") {
    cl.type <- "FORK"
  } else {
    cl.type <- "PSOCK"
  }

  cl <- makeCluster((detectCores() - free_threads), type = cl.type)

  clusterExport(cl = cl, varlist = c("network_el", "sim_SIR_unif", "beta", "gamma", "days", "iters"), envir = environment())

  sim_SIR_unif.out <- pblapply(c(1:length(network_el)), function(x,b,g,d,it) { print(replicate(it, (sim_SIR_unif(network_el[[x]],b,g,d)))) }, b = beta, g = gamma, d = days, it = iters, cl = cl)

  stopCluster(cl); return(sim_SIR_unif.out)

}
