#' calculate_metrics
#'
#' @param emp_el
#'
#' @return
#' @export
#'
#' @examples
calculate_metrics <- function(emp_el) {

  require(tnet); require(igraph); require(sna)

  metrics <- data.frame(index = NA, n = NA, mod = NA, dist = NA, diam = NA, clust = NA, cent = NA)
  metrics <- metrics[FALSE,]

  for (y in 1:length(emp_el)) {

    a <- emp_el[[y]]
    i <- a[,1]; j <- a[,2]; w <- a[,3]; b <- cbind(i, j, w)

    ig <- graph.data.frame(b, directed = FALSE)
    dens <- graph.density(ig) / 2
    cent <- igraph::evcent(ig, weights = w, scale = FALSE)
    Ce.ei <- (-sum(cent$vector - max(cent$vector))) / sum(rep(1, cent$options$n) - cent$vector)
    Newman.mod <- leading.eigenvector.community(ig, weights = w)$modularity
    diam <- diameter(ig, directed = FALSE, weights = w)
    size <- cent$options$n

    c <- symmetrise_w(b)
    Cl.wgm <- clustering_w(c, measure = "gm")
    dist.mat <- distance_w(c)
    dist.w <- mean(dist.mat, na.rm = TRUE)

    tmp <- data.frame(index = y, n = size, mod = Newman.mod, dist = dist.w, diam = diam, clust = Cl.wgm, cent = Ce.ei)

    metrics <- rbind(metrics, tmp)
  }

  return(metrics)
}
