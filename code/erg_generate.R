#------------#
# Generating #
#    ERGs    #
#------------#

samps = 111

nodes=c(10, 30, 50)
percents=c(0.15, 0.25, 0.35)

gnm_g<-list(NA)
gnm_el<-list(NA)

for (n in nodes) {

  p_gnm_g<-list(NA)
  p_gnm_el<-list(NA)

  for (p in percents) {

    m = round((n^2 - n)*p)
    g<-list(NA)
    el<-list(NA)

    i=1
    while(i < (samps + 1)) {
      g[[i]] <- sample_gnm(n, m, directed=FALSE, loops=FALSE)
      if(vertex_connectivity(g[[i]], checks = TRUE) > 0) {
        el[[i]] <- unique(get.edgelist(g[[i]]))
        el[[i]] <- cbind(el[[i]],weight=1)
        el[[i]] <- cbind(el[[i]],n_nodes=n)
        el[[i]] <- cbind(el[[i]],n_edges=m)
        i = i+1
      }
    }

    x=p*100
    p_gnm_g[[x]]<-g
    p_gnm_el[[x]]<-el
  }

  gnm_g[[n]]<-p_gnm_g
  gnm_el[[n]]<-p_gnm_el
}

saveRDS(gnm_g,file="Effective_g_erg")
saveRDS(gnm_el,file="Effective_el_erg")
save.image("Effective_Workspace.RData")
rm(gnm_g, gnm_el, p_gnm_g, p_gnm_el, x, g, el, i, m)
