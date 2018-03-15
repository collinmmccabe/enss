#' gen_erg
#'
#' gen_erg generates a set of Erdos-Renyi exponential random graphs of size "samples",
#'   repeated for each combination of the number of "nodes" and "percents" of edges
#'   present, provided in the function call
#'
#' @param samples the integer number of independent random graphs to be generated for
#'   each combination of nodes and percents
#' @param nodes an integer or vector of integers that indicate the size(s) of networks
#'   to be generated; if a list is supplied, graphs will be generated independently
#'   for each network size provided in the vector
#' @param percents a numeric or vector of numerics that indicate the percent(s) of
#'   possible edges present in networks to be generated; if a list is supplied,
#'   graphs will be generated independently for each percent provided in the vector
#'
#' @return list with 2 elements, the first of which (g) contains sociomatrices for
#'   each random graph, and the second of which (el) contains edgelists for those
#'   same graphs
#' @export
#'
#' @examples
gen_erg <- function(samples, nodes, percents) {

  gnm_g <- list(NA)
  gnm_el <- list(NA)

  for (n in nodes) {

    p_gnm_g <- list(NA)
    p_gnm_el <- list(NA)

    for (p in percents) {

      m <- round((n^2 - n)*p)
      g <- list(NA)
      el <- list(NA)

      i <- 1
      while(i < (samples + 1)) {
        g[[i]] <- igraph::sample_gnm(n, m, directed = FALSE, loops = FALSE)
        if(igraph::vertex_connectivity(g[[i]], checks = TRUE) > 0) {
          el[[i]] <- unique(get.edgelist(g[[i]]))
          el[[i]] <- cbind(el[[i]], weight = 1)
          el[[i]] <- cbind(el[[i]], n_nodes = n)
          el[[i]] <- cbind(el[[i]], n_edges = m)
          i <- i + 1
        }
      }

      x <- p * 100
      p_gnm_g[[x]] <- g
      p_gnm_el[[x]] <- el
    }

    gnm_g[[n]] <- p_gnm_g
    gnm_el[[n]] <- p_gnm_el
  }

  return(list(g = gnm_g, el = gnm_el))
}
