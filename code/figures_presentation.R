tiff("Figure1.tiff", height = 18, width = 18, units = 'cm', compression = "lzw", res = 300)

par(mar=c(1,1,1,1))
par(oma=c(0, 0, 0, 0))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

gnew <- graph.lattice( c(5,2) , circular=TRUE)
plot.igraph(gnew,  vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="(A) Circular Lattice")

Effective_g_max <- readRDS("./sims_maximal/max2/Effective_g_max2")
plot.igraph(Effective_g_max[[10]], layout = layout.circle, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="(B) Maximally-complete Graph")

Effective_g_erg <- readRDS("./sims_ERG/Effective_g_erg")
plot.igraph(Effective_g_erg[[10]][[25]][[59]], layout = layout.fruchterman.reingold, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="(C) Erdos-Renyi Random Graph")

Effective_g_emp <- readRDS("./sims_empirical/Effective_g_emp")
plot.igraph(Effective_g_emp[[4]], layout = layout.fruchterman.reingold, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="(D) Observed Primate Social Network")

# mtext("Comparison of Different Network Configurations,\nAll with 10 Nodes and Unweighted, Undirected Edges", font=2, side=3, outer=TRUE, line=0)

dev.off()


# Presentation Figures

par(mar=c(0,0,0,0))
par(oma=c(0,0,0,0))

plot.igraph(Effective_g_max[[30]], layout = layout.circle, vertex.size = 8, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=1, edge.color="black", edge.curved=F, main="")
# 12 days

plot.igraph(Effective_g_max[[10]], layout = layout.circle, vertex.size = 8, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=1, edge.color="black", edge.curved=F, main="")
# 7 days

plot.igraph(Effective_g_max[[15]], layout = layout.circle, vertex.size = 8, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=1, edge.color="black", edge.curved=F, main="")
# 9 days

plot.igraph(Effective_g_erg[[10]][[25]][[59]], layout = layout.fruchterman.reingold, vertex.size = 8, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=1.5, edge.color="black", edge.curved=F, main="")
# 9 days

plot.igraph(Effective_g_emp[[4]], layout = layout.fruchterman.reingold, vertex.size = 8, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=1.5, edge.color="black", edge.curved=F, main="")
# 12 days
