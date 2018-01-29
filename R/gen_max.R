#' gen_max
#'
#' gen_max generates a set of maximally complete graphs from size "nmin" to "nmax", provided in the function call
#'
#' @param nmin An integer of the lowest number of nodes to be generated as a maximally complete graph
#' @param nmax An integer of the highest number of nodes to be generated as a maximally complete graph
#'
#' @return list wirh 2 elements, the first of which (g) contains sociomatrices for each maximally complete graph, and the second of which (el) contains edgelists for those same graphs
#' @export
#'
#' @examples
gen_max <- function(nmin, nmax) {

  net0 <- list(NA); max_g <- list(NA); max_el <- list(NA)

  for(i in nmin:nmax) {

    net0[[i]] <- matrix(data = 1, nrow = i, ncol = i)
    diag(net0[[i]]) <- 0
    net0[[i]] <- network(net0[[i]])

    max_g[[i]] <- graph.edgelist(as.edgelist(net0[[i]])[,], directed = FALSE)

    max_el[[i]] <- cbind(unique(get.edgelist(max_g[[i]])), weight = 1, n_nodes = c(vcount(max_g[[i]]), rep(NA, ecount(max_g[[i]]) / 2 - 1)), n_edges = c(ecount(max_g[[i]]) / 2, rep(NA, ecount(max_g[[i]]) / 2 - 1)))

  }

  return(list(g = max_g, el = max_el))

}
