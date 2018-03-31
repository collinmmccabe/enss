#' gen_max
#'
#' gen_max generates a set of maximally complete graphs from size "n_min" to "n_max",
#'   provided in the function call
#'
#' @param n_min An integer of the lowest number of nodes to be generated as a maximally
#'   complete graph
#' @param n_max An integer of the highest number of nodes to be generated as a maximally
#'   complete graph
#'
#' @return list wirh 2 elements, the first of which (g) contains sociomatrices for each
#'   maximally complete graph, and the second of which (el) contains edgelists for those
#'   same graphs
#' @export
#'
#' @examples
gen_max <- function(n_min, n_max) {

  max_g <- list(NA); max_el <- list(NA)

  for(i in n_min:n_max) {

    index <- (i - n_min + 1)

    max_g[[index]] <- igraph::make_full_graph(i)

    max_el[[index]] <- cbind(igraph::as_edgelist(max_g[[index]]), weight = 1, n_nodes = c(igraph::vcount(max_g[[index]]), rep(NA, igraph::ecount(max_g[[index]]) - 1)), n_edges = c(igraph::ecount(max_g[[index]]), rep(NA, igraph::ecount(max_g[[index]]) - 1)))

  }

  return(list(g = max_g, el = max_el))

}
