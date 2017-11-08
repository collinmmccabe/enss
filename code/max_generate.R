#---------------------#
#     Generating      #
# Maximally-Complete  #
#      Networks       #
#---------------------#

nmin = 2
nmax = 300

net0 <- list(NA); max_g <- list(NA); max_el <- list(NA); sex_id <- list(NA)

for(i in nmin:nmax) {
  net0[[i]] <- matrix(data=1, nrow=i, ncol=i)
  diag(net0[[i]]) <- 0
  net0[[i]] <- network(net0[[i]])

  max_g[[i]] <- graph.edgelist(as.edgelist(net0[[i]])[,], directed=FALSE)
  sex_id[[i]] <- rep(1, vcount(max_g[[i]]))
  sex_id[[i]][unlist(by(1:length(sex_id[[i]]), sex_id[[i]], function(x) sample(x, ceiling(length(x)/2), FALSE)))] <- 2
  max_el[[i]] <- cbind(unique(get.edgelist(max_g[[i]])), weight=1, n_nodes=c(vcount(max_g[[i]]), rep(NA, ecount(max_g[[i]])/2-1)), n_edges=c(ecount(max_g[[i]])/2, rep(NA,ecount(max_g[[i]])/2-1)), sex_id=c(sex_id[[i]], rep(NA, ecount(max_g[[i]])/2-length(sex_id[[i]]))))
}

saveRDS(max_g,file="Effective_g_max")
saveRDS(max_el,file="Effective_el_max")
save.image("Effective_Workspace.RData")
rm(net0, max_g, max_el, sex_id, i)
