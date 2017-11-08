#####################################
# Effective Network Size Simulation #
#               - - -               #
#  Calculate SI/SIS/SIR/STD spread  #
#  time for maximal and E-G graphs  #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################

# Set working directory, depending on which OS I am using
switch(Sys.info()[['sysname']],
  Windows = {setwd("F:/Dropbox/Effective_Modeling/R/")},
  Linux = {setwd("/media/collin/HDD/Dropbox/Effective_Modeling/R/")},
  Darwin = {setwd("~/Dropbox/Effective_Modeling/R/")})

# Load multicore computing pkg, set number of cores to 2 less than max
library(parallel)
no_cores <- detectCores() - 2

# Load pkgs for network analyses
library(statnet)
library(igraph)

# Load pkg for model 2 regressions
library(lmodel2)

# Load pkg for 3D graphing
library(plot3D)


#----------------------#
#    Code for Model    #
#----------------------#

## 1. How does network size relate to saturation time?
#### 1.A generate maximally-complete, unweighted, undirected networks from 3:100 nodes

max_n = 50

net0 <- list(NA); netd <- list(NA); netud <- list(NA); networkud <- list(NA); networkd <- list(NA); sexd <- list(NA)

for(i in 6:max_n) {
  net0[[i]] <- matrix(data=1, nrow=i, ncol=i)
  diag(net0[[i]]) <- 0
  net0[[i]] <- network(net0[[i]])

  netud[[i]] <- graph.edgelist(as.edgelist(net0[[i]])[,], directed=FALSE)
  networkud[[i]] <- cbind(unique(get.edgelist(netud[[i]])), weight=1, n_nodes=c(vcount(netud[[i]]),rep(NA,ecount(netud[[i]])-1)), n_edges=c(ecount(netud[[i]])/2,rep(NA,ecount(netud[[i]])-1)))

  netd[[i]] <- graph.edgelist(as.edgelist(net0[[i]])[,], directed=TRUE)
  sexd[[i]] <- rep(1, vcount(netd[[i]]))
  sexd[[i]][unlist(by(1:length(sexd[[i]]), sexd[[i]], function(x) sample(x, ceiling(length(x)/2), FALSE)))] <- 2
  networkd[[i]] <- cbind(unique(get.edgelist(netd[[i]])), weight=1, n_nodes=c(vcount(netd[[i]]),rep(NA,ecount(netd[[i]])-1)), n_edges=c(ecount(netd[[i]]),rep(NA,ecount(netud[[i]])-1)), sexd=c(sexd[[i]],rep(NA,ecount(netd[[i]])-length(sexd[[i]]))))
}


#### 1.B simulate SI, SIS, SIR, and STD over each network for 1000 iterations (in parallel with as many cores as possible) to get reasonable means for saturation times

#------#
#  SI  #
#------#

sim_SI <- function(networkud, beta, intxn_per_day, days) {

  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  # run simulation for specified number of days
  day_counter <- 0
  while(day_counter <= days) {

    # Do this x times/day where x is number of interactions per day
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      selected_edge <- sample(1:e,1)

      # sum of infected (2) and susceptible (1) individuals is the only combination and will create a sum of three, so is used to identify potential transmission networks
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

      int_counter <- sum(int_counter,1)
    }

    day_counter <- sum(day_counter,1)
    if (sum(infection_status%%2) == 0) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}

#-------#
#  SIS  #
#-------#

sim_SIS <- function(networkud, beta, gamma, intxn_per_day, days) {

  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  # run simulation for specified number of days
  day_counter <- 0
  while(day_counter <= days) {

    # Do this x times/day where x is number of interactions per day
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      selected_edge <- sample(1:e,1)

      # sum of infected (2) and susceptible (1) individuals is the only combination and will create a sum of three, so is used to identify potential transmission networks
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

      int_counter <- sum(int_counter,1)
    }

    # at end of day, all 2's become 3's at a rate of gamma [i.e. if gamma is less than runif]; j should pick from indexes of all 2s
    for (j in which(infection_status %in% 2)) { # *** CM: This one was a little tricky, but I had to use a method to get the indices out of the infection_status vector. ***
      if (gamma >= runif(1,0,1)) { # *** CM: Same problem with greater than/less than... ***
        infection_status[j] = 1
      }
    }
    day_counter <- sum(day_counter,1)
    if (sum(infection_status%%2) == n) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}

#-------#
#  STD  #
#-------#

sim_STD <- function(networkd, beta, intxn_per_day, days, MM, MF, FM, FF) {

  n = networkd[1,4]
  e = networkd[1,5]
  cdata <- networkd[,1:2]
  sexes <- networkd[1:n,6]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  # run simulation for specified number of days
  day_counter <- 0
  while(day_counter <= days) {

    # Do this x times/day where x is number of interactions per day
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      selected_edge <- sample(1:e,1)

      # sum of infected (2) and susceptible (1) individuals is the only combination and will create a sum of three, so is used to identify potential transmission networks
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        sex_ind = 0; beta_mod = 0
        if (infection_status[cdata[selected_edge,1]] == 2) {
          sex_ind <- (sexes[cdata[selected_edge,1]] * 2) - sexes[cdata[selected_edge,2]] + 1
        } else {
          sex_ind <- (sexes[cdata[selected_edge,2]] * 2) - sexes[cdata[selected_edge,1]] + 1
        }

        switch(sex_ind, {beta_mod <- beta * MF}, {beta_mod <- beta * MM}, {beta_mod <- beta * FF}, {beta_mod <- beta * FM})

        if (beta_mod >= runif(1,0,1)) {
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

      int_counter <- sum(int_counter,1)
    }

    day_counter <- sum(day_counter,1)
    if (sum(infection_status%%2) == 0) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}

#-------#
#  SIR  #
#-------#

sim_SIR <- function(networkud, beta, gamma, intxn_per_day, days) {

  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  max_infected <- 1

  # run simulation for specified number of days
  day_counter <- 0
  while(day_counter <= days) {

    # Do this x times/day where x is number of interactions per day
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      selected_edge <- sample(1:e,1)

      # sum of infected (2) and susceptible (1) individuals is the only combination and will create a sum of three, so is used to identify potential transmission networks
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

      int_counter <- sum(int_counter,1)
    }

    # at end of day, all 2's become 3's at a rate of gamma [i.e. if gamma is less than runif]; j should pick from indexes of all 2s
    for (j in which(infection_status %in% 2)) { # *** CM: This one was a little tricky, but I had to use a method to get the indices out of the infection_status vector. ***
      if (gamma >= runif(1,0,1)) { # *** CM: Same problem with greater than/less than... ***
        infection_status[j] = 3
      }
    }

    curr_infected <- sum(infection_status == 2)
    if (curr_infected > max_infected) {
      max_infected <- curr_infected
    }

    day_counter <- sum(day_counter,1)

    if (sum(infection_status%%2) == n) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2),sum(infection_status == 3),max_infected))
}

#---------------#
#  Simulations  #
#---------------#

###### 1.B.1 record count of all Susceptibles in network at each time step of simulation
###### 1.B.2 do spread while Infected > 0
###### 1.B.3 record time step of when Susceptibles = 0, break
###### 1.B.4 if no more Infecteds, also break, since no more can be infected- record total Recovered at this point instead

beta = 0.2
MM = 4
MF = 1
FM = 0.5
FF = 0.1
gamma = 0.1
intxn_per_day = 3
days = 100
SIS_days = 20
iters = 1000

cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("networkud", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
SIR_max<-parLapply(cl, c(rep(6,5),6:max_n),function(x,b,g,i,d,it) print(replicate(it, (sim_SIR(networkud[[x]], b, g, i, d)))),b=beta,g=gamma,i=intxn_per_day,d=days,it=iters)
stopCluster(cl)

cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("networkud", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
SIS_max<-parLapply(cl, c(rep(6,5),6:max_n),function(x,b,g,i,d,it) print(replicate(it, (sim_SIS(networkud[[x]], b, g, i, d)))),b=beta,g=gamma,i=intxn_per_day,d=SIS_days,it=iters)
stopCluster(cl)

cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("networkud", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
SI_max<-parLapply(cl, c(rep(6,5),6:max_n),function(x,b,i,d,it) print(replicate(it, (sim_SI(networkud[[x]], b, i, d)))),b=beta,i=intxn_per_day,d=days,it=iters)
stopCluster(cl)

cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl=cl, varlist=c("networkd", "sim_STD", "beta", "intxn_per_day", "days", "MM", "MF", "FM", "FF", "iters"))
STD_max<-parLapply(cl, c(rep(6,5),6:max_n),function(x,b,i,d,mm,mf,fm,ff,it) print(replicate(it, (sim_STD(networkd[[x]], b, i, d, mm, mf, fm, ff)))),b=beta,i=intxn_per_day,d=days,mm=MM,mf=MF,fm=FM,ff=FF,it=iters)
stopCluster(cl)

saveRDS(SI_max,file="Effective_SI_max")
saveRDS(SIS_max,file="Effective_SIS_max")
saveRDS(SIR_max,file="Effective_SIR_max")
saveRDS(STD_max,file="Effective_STD_max")
save.image("/media/collin/HDD/Dropbox/EffectiveWorkspace.RData")

Effective_SI_max <- readRDS("Effective_SI_max")
Effective_SIS_max <- readRDS("Effective_SIS_max")
Effective_SIR_max <- readRDS("Effective_SIR_max")
Effective_STD_max <- readRDS("Effective_STD_max")

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

#--------#
#  ERGs  #
#--------#

## 2. Find independent axes of variation among network metrics of random networks.
#### 2.A generate 100 E-R graphs each for all combinations of network size (7,15,30,65,100) and completeness (30,50,70 percent) resulting in 15 sets of 100 graphs (7 nodes with 30% ties, 7 with 50%, 7 with 70%, 15 with 30%, etc.)

nodes=c(7, 15, 30, 65, 100)
percents=c(0.3, 0.5, 0.7)


#### 2.B create R list-type objects for each combination of network size and completeness
###### 2.B.1 store sociomatrix as 1st element of each record, then network size and completeness, leaving 5 elements NULL

gnm_g<-list(NA)
gnm_el<-list(NA)
for (n in nodes) {

  p_gnm_g<-list(NA)
  p_gnm_el<-list(NA)
  for (p in percents) {

    m = round((n^2 - n)*p/2)
    g<-list(NA)
    el<-list(NA)

    for(i in 1:100) {
      g[[i]] <- sample_gnm(n, m, directed=FALSE, loops=FALSE)
      el[[i]] <- unique(get.edgelist(g[[i]]))
      el[[i]] <- cbind(el[[i]],weight=1)
      el[[i]] <- cbind(el[[i]],n_nodes=n)
      el[[i]] <- cbind(el[[i]],n_edges=m)
    }

    x=p*100
    p_gnm_g[[x]]<-g
    p_gnm_el[[x]]<-el
  }

  gnm_g[[n]]<-p_gnm_g
  gnm_el[[n]]<-p_gnm_el
}

###### 2.B.2 calculate the following for each network: centralization, mean transitivity ratio, clustering coefficient, mean distance, modularity
###### 2.B.3 fill in the remaining elements of each record with the network metrics from 2.B.2

#### 2.C Test for statistical independence of the 5 network metrics and 2 traits (size and completeness)
###### 2.C.1 first, ensure that all metrics are normally distributed within their subset of size and completeness, if not, transform?
###### 2.C.2 run tests within metrics with size and completeness as repeated measures/random effects
###### 2.C.3 run tests between metrics & size and completeness across the entire dataset

#---------------#
#  Simulations  #
#    on ERGs    #
#---------------#

## 3. How do saturation times of random networks compare to maximally-complete ones?

#### 3.A use Latin hypercube/orthogonal sampling to select a representative subset of networks from part 2
###### 3.A.1 if certain measures are not independent of each other, include the combination of these non-independent variables as one orthogonal axis (or remove one from dataset)

#### 3.B repeat step 1.B for the new subset of E-R networks

SI_er<-list(NA)
SIS_er<-list(NA)
SIR_er<-list(NA)
for (n in nodes) {

  SI_tmp<-list(NA)
  SIS_tmp<-list(NA)
  SIR_tmp<-list(NA)
  for (p in (percents*100)) {
    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("gnm_el", "sim_SIR", "beta", "gamma", "intxn_per_day", "days", "iters"))
    SIR_tmp[[p]]<-parLapply(cl, c(1:100),function(x,b,g,i,d,it) print(replicate(it, (sim_SIR(gnm_el[[n]][[p]][[x]], b, g, i, d)))),b=beta,g=gamma,i=intxn_per_day,d=days,it=iters)
    stopCluster(cl)

    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("gnm_el", "sim_SIS", "beta", "gamma", "intxn_per_day", "SIS_days", "iters"))
    SIS_tmp[[p]]<-parLapply(cl, c(1:100),function(x,b,g,i,d,it) print(replicate(it, (sim_SIS(gnm_el[[n]][[p]][[x]], b, g, i, d)))),b=beta,g=gamma,i=intxn_per_day,d=SIS_days,it=iters)
    stopCluster(cl)

    cl <- makeCluster(no_cores, type="FORK")
    clusterExport(cl=cl, varlist=c("gnm_el", "sim_SI", "beta", "intxn_per_day", "days", "iters"))
    SI_tmp[[p]]<-parLapply(cl, c(1:100),function(x,b,i,d,it) print(replicate(it, (sim_SI(gnm_el[[n]][[p]][[x]], b, i, d)))),b=beta,i=intxn_per_day,d=days,it=iters)
    stopCluster(cl)
  }

  SI_er[[n]]<-SI_tmp
  SIS_er[[n]]<-SIS_tmp
  SIR_er[[n]]<-SIR_tmp
}


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


#### 3.C use AIC (or some other way of finding the best model) to get best predictors (metrics) of the ratio of "effective" network size to observed network size
#### 3.D using the predictors from step 3.C, generate new E-R graphs as in step 2.A and predict the effective network size
###### 3.D.1 compare (overlap, margin of error, etc.) these predictions to the saturation times generated by running SIR models on the networks (as in step 1.B)
###### 3.D.2 cross fingers, hope that this crazy idea of predicting disease spread from nothing but network metrics actually works!

# Bring in real networks from primate work
# Simulate disease spread on them to get estimates of effective network size
# run richness PGLS models with effective network size vs group size, see if effective net size fits better

test <- 42
