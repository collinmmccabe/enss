#' clust_sim_SI_w
#'
#' @param network_el
#' @param beta
#' @param intxn_per_day
#' @param days
#' @param iters
#' @param free_threads
#'
#' @return
#' @export
#'
#' @examples
clust_sim_SI_w <- function(network_el, beta, intxn_per_day, days, iters, free_threads = 1) {

  if (.Platform$OS.type == "unix") {
    cl.type <- "FORK"
  } else {
    cl.type <- "PSOCK"
  }

  cl <- makeCluster((detectCores() - free_threads), type = cl.type)

  clusterExport(cl = cl, varlist = c("network_el", "sim_SI_w", "beta", "intxn_per_day", "days", "iters"), envir = environment())

  sim_SI_w.out <- pblapply(c(1:length(network_el)), function(x,b,i,d,it) { print(replicate(it, (sim_SI_w(network_el[[x]],b,i,d)))) }, b = beta, i = intxn_per_day, d = days, it = iters, cl = cl)

  stopCluster(cl); return(sim_SI_w.out)

}
