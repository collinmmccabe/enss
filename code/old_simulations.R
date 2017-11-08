#####################################
# Effective Network Size Simulation #
#               - - -               #
#  Calculate SI/SIS/SIR/STD spread  #
#    time for maximally-complete,   #
#  Erdos-Renyi, & empirical graphs  #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

# Load multicore computing pkgs, set number of cores to 2 less than max
library(parallel)
library(pbapply)
no_cores <- detectCores() - 1

# Load pkgs for network analyses
library(statnet)
library(igraph)


################
#  Maximally-  #
#   Complete   #
#    Graphs    #
################

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


#----------------#
#   Simulation   #
#  on Maximally  #
#  Complete Net  #
#----------------#

Effective_g_max <- readRDS("Effective_g_max")
Effective_el_max <- readRDS("Effective_el_max")

MM = 4
MF = 1
FM = 0.5
FF = 0.1
intxn_per_day = 3
days = 10000
SIS_days = 200
iters = 1000

SI_maxsim <- list(NA); SIS_maxsim <- list(NA); SIR_maxsim <- list(NA); STD_maxsim <- list(NA)

for (beta in c(0.01, 0.025, 0.05, 0.075, 0.2)) {

  cat('\n', 'beta = ', beta, '\n', '\n')

  cat('\n', 'SI maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
  SI_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_max[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
  SI_maxsim[beta*1000] <- SI_max
  stopCluster(cl)

  cat('\n', 'STD maximal models:', '\n', '\n')
  cl <- makeCluster(no_cores, type="FORK")
  clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
  STD_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,i,d,mm,mf,fm,ff,it) { print(replicate(it, (sim_STD(Effective_el_max[[x]],b,i,d,mm,mf,fm,ff)))) }, b=beta, i=intxn_per_day, d=days, mm=MM, mf=MF, fm=FM, ff=FF, it=iters, cl=cl)
  STD_maxsim[beta*1000] <- STD_max
  stopCluster(cl)

  for (gamma in c(0.05, 0.1, 0.15)) {

    cat('\n', 'gamma = ', gamma, '\n', '\n')

    cat('\n', 'SIS maximal models:', '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
    SIS_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIS(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=SIS_days, it=iters, cl=cl)
    SIS_maxsim[beta*1000][gamma*100] <- SIS_max
    stopCluster(cl)

    cat('\n', 'SIR maximal models:', '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_max", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
    SIR_max <- pblapply(c(rep(nmin,(nmin-1)), nmin:nmax), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_max[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
    SIR_maxsim[beta*1000][gamma*100] <- SIR_max
    stopCluster(cl)
  }
}

saveRDS(SI_maxsim, file="Effective_SI_maxsim")
saveRDS(STD_maxsim, file="Effective_STD_maxsim")
saveRDS(SIS_maxsim, file="Effective_SIS_maxsim")
saveRDS(SIR_maxsim, file="Effective_SIR_maxsim")
#save.image("Effective_Workspace.RData")
rm(SI_max, SIS_max, STD_max, SIR_max, SI_maxsim, SIS_maxsim, STD_maxsim, SIR_maxsim)



#################
#  Erdos-Renyi  #
#    Graphs     #
#################

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


#---------------#
#  Simulations  #
#    on ERGs    #
#---------------#

# use Latin hypercube/orthogonal sampling to select a representative subset?

Effective_g_erg <- readRDS("Effective_g_erg")
Effective_el_erg <- readRDS("Effective_el_erg")

SI_erg<-list(NA)
##STD_erg<-list(NA)
##SIS_erg<-list(NA)
SIR_erg<-list(NA)

for (n in nodes) {

  SI_tmp<-list(NA)
##  STD_tmp<-list(NA)
##  SIS_tmp<-list(NA)
  SIR_tmp<-list(NA)

  for (p in (percents*100)) {

    cat('\n', 'SI ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
    SI_tmp[[p]] <- pblapply(c(1:samps), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_erg[[n]][[p]][[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
    stopCluster(cl)

##    cat('\n', 'STD ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
##    cl <- makeCluster(no_cores, type="FORK")
##    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
##    STD_tmp[[p]] <- pblapply(c(1:samps), function(x,b,i,d,mm,mf,fm,ff,it) { print(replicate(it, (sim_STD(Effective_el_erg[[n]][[p]][[x]],b,i,d,mm,mf,fm,ff)))) }, b=beta, i=intxn_per_day, d=days, mm=MM, mf=MF, fm=FM, ff=FF, it=iters, cl=cl)
##    stopCluster(cl)

##    cat('\n', 'SIS ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
##    cl <- makeCluster(no_cores, type="FORK")
##    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
##    SIS_tmp[[p]] <- pblapply(c(1:samps), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIS(Effective_el_erg[[n]][[p]][[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=SIS_days, it=iters, cl=cl)
##    stopCluster(cl)

    cat('\n', 'SIR ERG models:', '\n', 'nodes = ', n, ', percent = ', p, '\n', '\n')
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("Effective_el_erg", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
    SIR_tmp[[p]] <- pblapply(c(1:samps), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_erg[[n]][[p]][[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
    stopCluster(cl)
  }

  SI_erg[[n]]<-SI_tmp
##  STD_erg[[n]]<-STD_tmp
##  SIS_erg[[n]]<-SIS_tmp
  SIR_erg[[n]]<-SIR_tmp
}

saveRDS(SI_erg,file="Effective_SI_erg")
##saveRDS(STD_erg,file="Effective_STD_erg")
##saveRDS(SIS_erg,file="Effective_SIS_erg")
saveRDS(SIR_erg,file="Effective_SIR_erg")
save.image("Effective_Workspace.RData")
rm(SI_erg, SIR_erg, n, p)



###############
#  Empirical  #
#   Primate   #
#   Networks  #
###############

#-----------#
# Importing #
# Empirical #
#  Networks #
#-----------#

# Bring in real networks from primate work
# Simulate disease spread on them to get estimates of effective network size
# run richness PGLS models with effective network size vs group size
# see if effective net size fits better

setwd("empirical_edgelists/")
input_names = list.files()

emp_g = list(NA); emp_el = list(NA); spp_list = data.frame(filename=NA, spp=NA)
for(i in 1:length(input_names)) {
  cat('\n', 'File name:', input_names[i])
  tmp <- read.csv(input_names[i], header = F)

  emp_g[[i]] <- graph.edgelist(as.matrix(tmp[,1:2]), directed=FALSE)
  emp_el[[i]] <- cbind(unique(get.edgelist(emp_g[[i]])), weight=1, n_nodes=c(vcount(emp_g[[i]]), rep(NA, ecount(emp_g[[i]])/2-1)), n_edges=c(ecount(emp_g[[i]])/2, rep(NA, ecount(emp_g[[i]])/2-1)))

  spp_list[i,] <- c(input_names[i], NA)

##  plot(emp_g[[i]])
  cat('\n', 'Correctly calculated edge count:', nrow(emp_el[[i]])==emp_el[[i]][1,5], '\n')
}

spp_list[,2] <- c("Alouatta_caraya", "Alouatta_guariba", "Ateles_geoffroyi", "Pan_paniscus", "Brachyteles_arachnoides", "Callithrix_jacchus", "Trachypithecus_pileatus", "Cebus_apella", "Cebus_capucinus", "Cercopithecus_aethiops", "Cercopithecus_campbelli", "Cercopithecus_mitis", "Pan_troglodytes", "Colobus_guereza", "Erythrocebus_patas", "Petterus_fulvus", "Macaca_fuscata", "Macaca_fuscata", "Semnopithecus_entellus", "Lemur_catta", "Macaca_arctoides", "Macaca_assamensis", "Macaca_fascicularis", "Macaca_fuscata", "Macaca_mulatta", "Macaca_radiata", "Macaca_tonkeana", "Mandrillus_sphinx", "Miopithecus_talapoin", "Macaca_mulatta", "Pan_paniscus", "Pan_troglodytes", "Papio_hamadryas", "Papio_ursinus", "Papio_papio", "Papio_ursinus", "Nasalis_larvartus", "Saguinus_fuscicollis", "Saguinus_mystax", "Saimiri_sciureus", "Theropithecus_gelada", "Trachypithecus_johnii")

switch(Sys.info()[['sysname']],
       Windows = {setwd("D:/Dropbox/Effective_Modeling/R/")},
       Linux = {setwd("/media/collin/SSD/Dropbox/Effective_Modeling/R/")},
       Darwin = {setwd("~/Dropbox/Effective_Modeling/R/")})

saveRDS(emp_g,file="Effective_g_emp")
saveRDS(emp_el,file="Effective_el_emp")
saveRDS(spp_list,file="Effective_spp_emp")
save.image("Effective_Workspace.RData")
rm(emp_g, emp_el, input_names, i, tmp)


#----------------#
#   Simulation   #
#  on Empirical  #
#    Networks    #
#----------------#

Effective_g_emp <- readRDS("Effective_g_emp")
Effective_el_emp <- readRDS("Effective_el_emp")

cat('\n', 'SI empirical models:', '\n', '\n')
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
SI_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,i,d,it) { print(replicate(it, (sim_SI(Effective_el_emp[[x]],b,i,d)))) }, b=beta, i=intxn_per_day, d=days, it=iters, cl=cl)
stopCluster(cl)

##cat('\n', 'STD empirical models:', '\n', '\n')
##cl <- makeCluster(no_cores, type="FORK")
##clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
##STD_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,i,d,mm,mf,fm,ff,it) { print(replicate(it, (sim_STD(Effective_el_emp[[x]],b,i,d,mm,mf,fm,ff)))) }, b=beta, i=intxn_per_day, d=days, mm=MM, mf=MF, fm=FM, ff=FF, it=iters, cl=cl)
##stopCluster(cl)

##cat('\n', 'SIS empirical models:', '\n', '\n')
##cl <- makeCluster(no_cores, type="FORK")
##clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
##SIS_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIS(Effective_el_emp[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=SIS_days, it=iters, cl=cl)
##stopCluster(cl)

cat('\n', 'SIR empirical models:', '\n', '\n')
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("Effective_el_emp", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
SIR_emp <- pblapply(c(1:length(Effective_el_emp)), function(x,b,g,i,d,it) { print(replicate(it, (sim_SIR(Effective_el_emp[[x]],b,g,i,d)))) }, b=beta, g=gamma, i=intxn_per_day, d=days, it=iters, cl=cl)
stopCluster(cl)

saveRDS(SI_emp,file="Effective_SI_emp")
##saveRDS(STD_emp,file="Effective_STD_emp")
##saveRDS(SIS_emp,file="Effective_SIS_emp")
saveRDS(SIR_emp,file="Effective_SIR_emp")
save.image("Effective_Workspace.RData")
rm(SI_emp, SIR_emp)



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
