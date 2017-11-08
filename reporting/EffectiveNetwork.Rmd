---
title: "Effective Network Size"
author: "Collin McCabe"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: false
  html_document:
    theme: flatly
    toc: true
    number_section: true
---

```{r include=FALSE, echo=FALSE}
library(statnet)
library(scatterplot3d)
library(stargazer)
library(igraph)
library(lmodel2)
setwd("/home/collin/HDD/Dropbox backup/Effective_Modeling/R/")
```

# Network size and saturation time in null model
## Null Model: Maximally-complete, unweighted networks

First, we generated sociomatrices with '1' signifying ties between a row index and a column index.  Because we were focused on social networks, we replaced the diagonals of the matrices with '0' to show that there are no self-referencing, or looping ties.

```{r}
  net <- matrix(data=1, nrow=6, ncol=6)
  diag(net) <- 0
```

```{r echo=FALSE}
net
```

We then generated a `statnet` network-type object to facilitate network-specific analyses.

```{r}
  net <- network(net)
```

\newpage
And we represented these as edgelists to make referencing random edges for disease spread more intuitive. We calculated the number of edges and vertices for each network as well, to reduce computation time during simulations.
```{r}
  netud <- graph.edgelist(as.edgelist(net)[,], directed=FALSE)
  networkud <- cbind(unique(get.edgelist(netud)), weight=1,
                     n_nodes=c(vcount(netud), rep(NA, ecount(netud)/2-1)),
                     n_edges=c(ecount(netud)/2, rep(NA, ecount(netud)/2-1)))
```

```{r echo=FALSE}
networkud
max_n=50
```

We generated these graphs for network sizes from 6 (the minimum number of nodes required for modular networks) to 50.  

## Transmission Modes

Once we had our sample of null-model (maximally complete) networks, we simulated the spread of generic diseases.  All diseases shared the same parameters for beta (the infection rate, or how likely a susceptible individual was to become infected upon interaction with an infected individual), gamma where applicable (the recovery rate, or the daily likelihood than an infected individual would recover to either a resistant state- for SIR, or a susceptible state- for SIS), per captia interaction rate per day (scaling with network size), and the number of days for which a disease would be simulated.  For STD models, we also included infection rate modifiers, because transmission rate of an STD is dependent on the sex of the infected individual and the sex of the susceptible: 


 Infected ->  | Male | Female 
-------------:|------|--------
     Male     |  3   |  0.5   
    Female    |  1   |  0.1   


We used iterative (looping), edge-selection-based algorithms for simulating the spread of disease through our populations.  The algorithms for each of the disease modes are listed below.

\newpage
### SIS

SIS models assume that after a period of time, infected individuals will return to a susceptible state.  So, as long as at least one individual in the group is infected at anyone time, the proportion of infected individuals to susceptible individuals will tend to oscillate around an equilibrium level.  As this is the case, we are more concerned with the equilibrium ratio of infecteds to susceptibles in the population, and this is returned from the following function.

```{r}
sim_SIS <- function(networkud, beta, gamma, intxn_per_day, days) {
  
  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]
  
  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2
  
  day_counter <- 0
  while(day_counter <= days) {
    
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {
      
      selected_edge <- sample(1:e,1)

      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) { 
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }
      
      int_counter <- sum(int_counter,1)
    }
    
    for (j in which(infection_status %in% 2)) { 
      if (gamma >= runif(1,0,1)) {
        infection_status[j] = 1
      }
    }
    day_counter <- sum(day_counter,1)
    if (sum(infection_status%%2) == n) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}
```

\newpage
### SI

SI diseases assume no recovery, and so these will in theory spread to every susceptible individual in a population eventually, as long as individuals do not die and all individuals are connected in the network.  In order to make sure that our diseases are reaching every individual, we reported the final infected ratio, assuming that all will reach 100%.

```{r}
sim_SI <- function(networkud, beta, intxn_per_day, days) {
  
  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]
  
  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2
  
  day_counter <- 0
  while(day_counter <= days) {
    
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {
      
      selected_edge <- sample(1:e,1)
      
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
```

\newpage
### STD

STDs are really just special cases of SI diseases, where the transmission rate varies based on the sex of interacting individuals.  The beta modifiers for each type of interaction (male-to-female, MM; female-to-male, FM; male-to-male, MM; and female-to-female, FF) are therefore listed as additional inputs for the STD simulation function.

```{r}
sim_STD <- function(networkd, beta, intxn_per_day, days, MM, MF, FM, FF) {
  
  n = networkd[1,4]
  e = networkd[1,5]
  cdata <- networkd[,1:2]
  sexes <- networkd[1:n,6]
  
  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2
  
  day_counter <- 0
  while(day_counter <= days) {
    
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {
      
      selected_edge <- sample(1:e,1)
      
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        sex_ind = 0; beta_mod = 0
        if (infection_status[cdata[selected_edge,1]] == 2) {
          sex_ind <- (sexes[cdata[selected_edge,1]] * 2) - sexes[cdata[selected_edge,2]]+1
        } else {
          sex_ind <- (sexes[cdata[selected_edge,2]] * 2) - sexes[cdata[selected_edge,1]]+1
        }
        
        switch(sex_ind, {beta_mod <- beta * MF}, {beta_mod <- beta * MM},
               {beta_mod <- beta * FF}, {beta_mod <- beta * FM})
        
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
```

\newpage
### SIR

Finally, the most complex model we tested, SIR, assumes that after a period of infection, individuals recover and become immune to future infection.  For this reason, we recorded the maximum number of individuals infected at any point in the simulation to gauge the peak prevalence of the disease.

```{r}
sim_SIR <- function(networkud, beta, gamma, intxn_per_day, days) {
  
  n = networkud[1,4]
  e = networkud[1,5]
  cdata <- networkud[,1:2]
  
  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2
  
  max_infected <- 1
  
  day_counter <- 0
  while(day_counter <= days) {
    
    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {
      
      selected_edge <- sample(1:e,1)
      
      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) { 
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }
      
      int_counter <- sum(int_counter,1)
    }

    for (j in which(infection_status %in% 2)) { 
      if (gamma >= runif(1,0,1)) {
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
  return(c(day_counter-1, sum(infection_status == 1), sum(infection_status == 2),
           sum(infection_status == 3), max_infected))
}
```

\newpage
## Correlations between Network Size, Spread Time, and Prevalence

We tested for associations between the size of a network and both a.) time until extinction, saturation, or equilibrium of a disease as well as b.) maximum prevalence of the disease.  We tested these using ordinary least squares and model 2 regression models. The relationships were not quite as tight as hoped, but do still show general trends toward predictive relationships.  The results are broken up again by transmission mode.

### SIS results

#### Graph

```{r echo=FALSE}
Effective_SIS_max <- readRDS("Effective_SIS_max")
SIS_max_df <- data.frame(days=Effective_SIS_max[[6]][1,], S_final=Effective_SIS_max[[6]][2,], I_final=Effective_SIS_max[[6]][3,], n=6)
for (i in 7:max_n) {
  SIS_max_df <- rbind(SIS_max_df, cbind(days=Effective_SIS_max[[i]][1,], S_final=Effective_SIS_max[[i]][2,], I_final=Effective_SIS_max[[i]][3,], n=i))
}
SIS_max_df <- cbind(SIS_max_df, equilibrium=(SIS_max_df$I_final / SIS_max_df$n))
scatterplot3d(data.frame(days=SIS_max_df$days, eq=SIS_max_df$equilibrium, n=SIS_max_df$n),
              main="SIS Model Outcomes\n(Results of 1000 Iterations)",
              xlab = "Outbreak Time (days)",
              ylab = "Prevalence of Infection at Equilibrium",
              zlab = "Network size (n)")
```

#### Linear Models

```{r results='asis', echo=FALSE, message=FALSE}
modSIS.1=lm(n ~ days + equilibrium, data=SIS_max_df)
modSIS.2=lm(n ~ days, data=SIS_max_df)
modSIS.3=lm(n ~ equilibrium, data=SIS_max_df)
stargazer(modSIS.1, modSIS.2, modSIS.3, header=FALSE, title = "SIS Linear Models", omit.stat="f", intercept.bottom=FALSE,  covariate.labels=c("Intercept", "Outbreak Duration (days)", "Equilibrium Number of Infected (n)"), dep.var.labels="Network Size (n)")
```

```{r message=FALSE}
lmodel2(n ~ days + equilibrium, data=SIS_max_df)
```


These results suggest that maybe we should separate instances of disease going extinct vs reaching equilibrium, as these show distinct splits in the graph

\newpage
### SI results

#### Graph

```{r echo=FALSE}
Effective_SI_max <- readRDS("Effective_SI_max")
SI_max_df <- data.frame(days=Effective_SI_max[[6]][1,], S_final=Effective_SI_max[[6]][2,], I_final=Effective_SI_max[[6]][3,], n=6)
for (i in 7:max_n) {
  SI_max_df <- rbind(SI_max_df, cbind(days=Effective_SI_max[[i]][1,], S_final=Effective_SI_max[[i]][2,], I_final=Effective_SI_max[[i]][3,], n=i))
}
scatterplot3d(data.frame(days=SI_max_df$days, eq=SI_max_df$I_final, n=SI_max_df$n),
              main="SI Model Outcomes\n(Results of 1000 Iterations)",
              xlab = "Outbreak Time (days)",
              ylab = "Final Prevalence of Infection",
              zlab = "Network size (n)")
```

#### Linear Models

```{r results='asis', echo=FALSE, message=FALSE}
modSI.1=lm(n ~ days + I_final, data=SI_max_df)
modSI.2=lm(n ~ days, data=SI_max_df)
modSI.3=lm(n ~ I_final, data=SI_max_df)
stargazer(modSI.1, modSI.2, modSI.3, header=FALSE, title = "SI Linear Models", omit.stat="f", intercept.bottom=FALSE,  covariate.labels=c("Intercept", "Outbreak Duration (days)", "Final Number of Infected (n)"), dep.var.labels="Network Size (n)")
```

```{r message=FALSE}
lmodel2(n ~ days + I_final, data=SI_max_df)
```

In the SI models, I think beta might be too high to get good estimates of spread time, since everything appears to go to saturation so quickly.

\newpage
### STD results

#### Graph

```{r echo=FALSE}
Effective_STD_max <- readRDS("Effective_STD_max")
STD_max_df <- data.frame(days=Effective_STD_max[[6]][1,], S_final=Effective_STD_max[[6]][2,], I_final=Effective_STD_max[[6]][3,], n=6)
for (i in 7:max_n) {
  STD_max_df <- rbind(STD_max_df, cbind(days=Effective_STD_max[[i]][1,], S_final=Effective_STD_max[[i]][2,], I_final=Effective_STD_max[[i]][3,], n=i))
}
scatterplot3d(data.frame(days=STD_max_df$days, eq=STD_max_df$I_final, n=STD_max_df$n),
              main="STD Model Outcomes\n(Results of 1000 Iterations)",
              xlab = "Outbreak Time (days)",
              ylab = "Final Prevalence of Infection",
              zlab = "Network size (n)")
```

#### Linear Models

```{r results='asis', echo=FALSE, message=FALSE}
modSTD.1=lm(n ~ days + I_final, data=STD_max_df)
modSTD.2=lm(n ~ days, data=STD_max_df)
modSTD.3=lm(n ~ I_final, data=STD_max_df)
stargazer(modSTD.1, modSTD.2, modSTD.3, header=FALSE, title = "STD Linear Models", omit.stat="f", intercept.bottom=FALSE,  covariate.labels=c("Intercept", "Outbreak Duration (days)", "Final Number of Infected (n)"), dep.var.labels="Network Size (n)")
```

```{r message=FALSE}
lmodel2(n ~ days + I_final, data=STD_max_df)
```

...again, the STD results might indicate an issue of beta being too high.

\newpage
### SIR results

#### Graph

```{r echo=FALSE}
Effective_SIR_max <- readRDS("Effective_SIR_max")
SIR_max_df <- data.frame(days=Effective_SIR_max[[6]][1,], S_final=Effective_SIR_max[[6]][2,], I_final=Effective_SIR_max[[6]][3,], R_final=Effective_SIR_max[[6]][4,], I_max=Effective_SIR_max[[6]][5,], n=6)
for (i in 7:max_n) {
  SIR_max_df <- rbind(SIR_max_df, cbind(days=Effective_SIR_max[[i]][1,], S_final=Effective_SIR_max[[i]][2,], I_final=Effective_SIR_max[[i]][3,], R_final=Effective_SIR_max[[i]][4,], I_max=Effective_SIR_max[[i]][5,], n=i))
}
scatterplot3d(data.frame(days=SIR_max_df$days, eq=SIR_max_df$I_max, n=SIR_max_df$n),
              main="SIR Model Outcomes\n(Results of 1000 Iterations)",
              xlab = "Outbreak Time (days)",
              ylab = "Maximum Prevalence of Infection",
              zlab = "Network size (n)")
```

#### Linear Models

Should we ignore simulations with early extinctions, because again, there is grouping of those results on the graph?  Or include proportion of iterations going extinct as another variable for prediction?

```{r results='asis', echo=FALSE, message=FALSE}
modSIR.1=lm(n ~ days + I_max, data=SIR_max_df)
modSIR.2=lm(n ~ days, data=SIR_max_df)
modSIR.3=lm(n ~ I_max, data=SIR_max_df)
stargazer(modSIR.1, modSIR.2, modSIR.3, header=FALSE, title="SIR Linear Models", omit.stat="f", intercept.bottom=FALSE,  covariate.labels=c("Intercept", "Outbreak Duration (days)", "Maximum Infected at once (n)"), dep.var.labels="Network Size (n)")
```

```{r message=FALSE}
lmodel2(n ~ days + I_max, data=SIR_max_df)
```