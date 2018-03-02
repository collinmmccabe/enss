#' clust_sim_SI_unif
#'
#' @param network_el
#' @param beta
#' @param days
#' @param iters
#' @param free_threads
#'
#' @return
#' @export
#'
#' @examples
clust_sim_SI_unif <- function(network_el, beta, days, iters, free_threads = 1) {

  if (.Platform$OS.type == "unix") {
    cl.type <- "FORK"
  } else {
    cl.type <- "PSOCK"
  }

  cl <- makeCluster((detectCores() - free_threads), type = cl.type)

  clusterExport(cl = cl, varlist = c("network_el", "sim_SI_unif", "beta", "days", "iters"), envir = environment())

  sim_SI_unif.out <- pblapply(c(1:length(network_el)), function(x,b,d,it) { print(replicate(it, (sim_SI_unif(network_el[[x]],b,d)))) }, b = beta, d = days, it = iters, cl = cl)

  stopCluster(cl); return(sim_SI_unif.out)

}
