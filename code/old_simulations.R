#####################################
# Effective Network Size Simulation #
#               - - -               #
#  Calculate SI/SIS/SIR/STD spread  #
#    time for maximally-complete,   #
#  Erdos-Renyi, & empirical graphs  #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

# Load multicore computing pkgs
library(parallel)
library(pbapply)

# Load pkgs for network analyses
library(statnet)
library(igraph)
library(tnet)

# Load pkg for model 2 regressions
library(lmodel2)

# Load pkg for 3D graphing
library(plot3D)

# Load pkg for comparing distributions
library(segmented)
library(dgof)

# Load pkgs for graphing
library(ggplot2)
library(grid)
library(gridExtra)
library(plotrix)

# Load pkgs for pgls
library(caper)
library(dplyr)
library(MuMIn)


# Weighted ERG generation code

cdata_tnet <- as.tnet(cdata, type="weighted one-mode tnet")
rg_reshuffling_w(cdata_tnet, option="links", directed=0)
tnet_igraph(cdata_tnet, type="weighted one-mode tnet", directed=0)

# GRAPHS

par(mar(1,1,1,1))
par(oma=c(0, 0, 3, 0))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

gnew <- graph.lattice( c(5,2) , circular=TRUE)
plot.igraph(gnew,  vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="A. Circular Lattice")

plot.igraph(Effective_g_max[[10]], layout = layout.circle, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="B. Maximally-complete Graph")

plot.igraph(Effective_g_erg[[10]][[25]][[59]], layout = layout.fruchterman.reingold, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="C. Erdos-Renyi Random Graph")

plot.igraph(Effective_g_emp[[4]], layout = layout.fruchterman.reingold, vertex.size = 15, vertex.color="red", vertex.frame.color= "white", vertex.label=NA, edge.width=2, edge.color="black", edge.curved=F, main="D. Observed Primate Social Network")

mtext("Comparison of Different Network Configurations,\nAll with 10 Nodes and Unweighted, Undirected Edges", font=2, side=3, outer=TRUE, line=0)


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




#---------------#
#  Graphs from  #
#  Simulations  #
#---------------#

SIR_max_df <- data.frame(days=Effective_SIR_max[[6]][1,], S_final=Effective_SIR_max[[6]][2,], I_final=Effective_SIR_max[[6]][3,], R_final=Effective_SIR_max[[6]][4,], I_max=Effective_SIR_max[[6]][5,], n=6)
for (i in 7:max_n) {
  SIR_max_df <- rbind(SIR_max_df, cbind(days=Effective_SIR_max[[i]][1,], S_final=Effective_SIR_max[[i]][2,], I_final=Effective_SIR_max[[i]][3,], R_final=Effective_SIR_max[[i]][4,], I_max=Effective_SIR_max[[i]][5,], n=i))
}

scatter3D(SIR_max_df$days, SIR_max_df$I_max, SIR_max_df$n)
scatter3D(SIR_max_df$I_max, SIR_max_df$days, SIR_max_df$n)
# ignore early extinctions?


SIS_max_df <- data.frame(days=Effective_SIS_max[[6]][1,], S_final=Effective_SIS_max[[6]][2,], I_final=Effective_SIS_max[[6]][3,], n=6)
for (i in 7:max_n) {
  SIS_max_df <- rbind(SIS_max_df, cbind(days=Effective_SIS_max[[i]][1,], S_final=Effective_SIS_max[[i]][2,], I_final=Effective_SIS_max[[i]][3,], n=i))
}

SIS_max_df <- cbind(SIS_max_df, equilibrium=(SIS_max_df$I_final / SIS_max_df$n))
scatter3D(SIS_max_df$days, SIS_max_df$equilibrium, SIS_max_df$n)
scatter3D(SIS_max_df$equilibrium, SIS_max_df$days, SIS_max_df$n)
# separate disease going extinct vs reaching equilibrium?


SI_max_df <- data.frame(days=Effective_SI_max[[6]][1,], S_final=Effective_SI_max[[6]][2,], I_final=Effective_SI_max[[6]][3,], n=6)
for (i in 7:max_n) {
  SI_max_df <- rbind(SI_max_df, cbind(days=Effective_SI_max[[i]][1,], S_final=Effective_SI_max[[i]][2,], I_final=Effective_SI_max[[i]][3,], n=i))
}

scatter3D(SI_max_df$days, SI_max_df$I_final, SI_max_df$n)
scatter3D(SI_max_df$I_final, SI_max_df$days, SI_max_df$n)
# beta might be too high to get good estimates here, everything goes to saturation so quickly


STD_max_df <- data.frame(days=Effective_STD_max[[6]][1,], S_final=Effective_STD_max[[6]][2,], I_final=Effective_STD_max[[6]][3,], n=6)
for (i in 7:max_n) {
  STD_max_df <- rbind(STD_max_df, cbind(days=Effective_STD_max[[i]][1,], S_final=Effective_STD_max[[i]][2,], I_final=Effective_STD_max[[i]][3,], n=i))
}

scatter3D(STD_max_df$days, STD_max_df$I_final, STD_max_df$n)
scatter3D(STD_max_df$I_final, STD_max_df$days, STD_max_df$n)
# again, there may be a problem with beta being too high...


#### 1.C linear model of network size as independent, saturation time as dependent
###### 1.C.1 test various fits for the relationship (within reason)

# SIR
summary(lm(n ~ days + I_max, data=SIR_max_df))
lmodel2(n ~ days + I_max, data=SIR_max_df)

# SIS
summary(lm(n ~ days + equilibrium, data=SIS_max_df))
lmodel2(n ~ days + equilibrium, data=SIS_max_df)

# SI
summary(lm(n ~ days + I_final, data=SI_max_df))
lmodel2(n ~ days + I_final, data=SI_max_df)

# STD
summary(lm(n ~ days + I_final, data=STD_max_df))
lmodel2(n ~ days + I_final, data=STD_max_df)

# ERG analysis

for (n in nodes) {
  for (p in percents*100) {
    for (i in 1:100) {
      print(transitivity(gnm_g[[n]][[(p)]][[i]])) } } }

for (n in nodes) {
  for (p in percents*100) {
    for (i in 1:100) {
      print(infomap.community(gnm_g[[n]][[p]][[i]], nb.trials = 10, modularity = TRUE)$mod) } } }


###### 3.B.1 plug the saturation time into the inverse of the linear model result from step 1.C to determine the estimated "effective" network size

model_01<- lmodel2(y ~ x)
