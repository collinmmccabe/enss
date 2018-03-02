#' clust_sim_SIR
#'
#' @param network_el
#' @param beta
#' @param gamma
#' @param intxn_per_day
#' @param days
#' @param iters
#' @param free_threads
#'
#' @return
#' @export
#'
#' @examples
clust_sim_SIR <- function(network_el, beta, gamma, intxn_per_day, days, iters, free_threads = 1) {

  if (.Platform$OS.type == "unix") {
    cl.type <- "FORK"
  } else {
    cl.type <- "PSOCK"
  }

  cl <- makeCluster((detectCores() - free_threads), type = cl.type)

  clusterExport(cl = cl, varlist = c("network_el", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"), envir = environment())

  sim_SIR.out <- pblapply(c(1:length(network_el)), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(network_el[[x]],b,g,i,d)))) }, b = beta, g = gamma, i = intxn_per_day, d = days, it = iters, cl = cl)

  stopCluster(cl); return(sim_SIR.out)

}
