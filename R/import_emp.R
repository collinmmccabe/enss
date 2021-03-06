#' import_emp
#'
#' import_emp imports empirical edgelists within a single folder and converts
#'   them into a format intelligible to the rest of the functions in enss
#'
#' @param folder The path to a folder with (only) edgelists inside of it
#'
#' @return list with 4 elements, the first (g) contains sociomatrices for each
#'   empirical graph, the second (el) contains edgelists for those same graphs,
#'   the third (el_w) contains weighted edglists of those same graphs, and the
#'   fourth (spp) contains a dataframe of the filenames of each edgelist imported
#'   by this function, to later be filled in by the user with the species names
#'   for each edgelist
#' @export
#'
#' @examples
import_emp <- function(folder) {

  input_names <- list.files(folder)

  emp_g <- list(NA); emp_el <- list(NA); emp_el_w <- list(NA); spp_list <- data.frame(filename = NA, spp = NA)

  for(i in 1:length(input_names)) {
    tmp <- read.csv(paste(folder, input_names[i], sep = ""), header = FALSE)

    emp_g[[i]] <- igraph::graph_from_edgelist(as.matrix(tmp[,1:2]), directed = FALSE)

    emp_el[[i]] <- cbind(unique(igraph::get.edgelist(emp_g[[i]])), weight = 1, n_nodes = c(igraph::vcount(emp_g[[i]]), rep(NA, igraph::ecount(emp_g[[i]]) / 2 - 1)), n_edges = c(igraph::ecount(emp_g[[i]]) / 2, rep(NA, igraph::ecount(emp_g[[i]]) / 2 - 1)))

    emp_el_w[[i]] <- cbind(unique(igraph::get.edgelist(emp_g[[i]])), weight = NA, n_nodes = c(igraph::vcount(emp_g[[i]]), rep(NA, igraph::ecount(emp_g[[i]]) / 2 - 1)), n_edges = c(igraph::ecount(emp_g[[i]]) / 2, rep(NA, igraph::ecount(emp_g[[i]]) / 2 - 1)))

    for (j in 1:emp_el_w[[i]][1,5]) {
      emp_el_w[[i]][j,3] <- tmp[which(tmp[,1] == emp_el_w[[i]][j,1] & tmp[,2] == emp_el_w[[i]][j,2]), 3]
    }

    spp_list[i,] <- c(input_names[i], NA)
  }

  rm(input_names, i, tmp)
  return(list(g = emp_g, el = emp_el, el_w = emp_el_w, spp = spp_list))
}
