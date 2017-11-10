##################
#    Disease     #
#  Transmission  #
#   Algorithms   #
##################


#------#
#  SI  #
#------#

sim_SI <- function(network_el, beta, intxn_per_day, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:2]

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

    day_counter = day_counter+1
    if (sum(infection_status%%2) == 0) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}


#-------#
#  SIR  #
#-------#

sim_SIR <- function(network_el, beta, gamma, intxn_per_day, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:2]

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
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2),sum(infection_status == 3),max_infected))
}


#---------------#
#  Weighted SI  #
#---------------#

sim_SI_w <- function(network_el, beta, intxn_per_day, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:3]
  e <- cdata[,3]
  edgeweight_sum <- sum(e)

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  day_counter <- 0
  while(day_counter <= days) {

    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      r <- runif(1,0,edgeweight_sum)
      for (i in 1:length(e)) {
        if (sum(e[1:i]) > r) {
          selected_edge = i
          break
        }
      }

      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

      int_counter <- sum(int_counter,1)
    }

    day_counter = day_counter+1
    if (sum(infection_status%%2) == 0) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}


#----------------#
#  Weighted SIR  #
#----------------#

sim_SIR_w <- function(network_el, beta, gamma, intxn_per_day, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:3]
  e <- cdata[,3]
  edgeweight_sum <- sum(e)

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  max_infected <- 1

  day_counter <- 0
  while(day_counter <= days) {

    int_counter <- 0
    while(int_counter <= intxn_per_day*n) {

      r <- runif(1,0,edgeweight_sum)
      for (i in 1:length(e)) {
        if (sum(e[1:i]) > r) {
          selected_edge = i
          break
        }
      }

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
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2),sum(infection_status == 3),max_infected))
}


#--------------#
#  Uniform SI  #
#--------------#

sim_SI_unif <- function(network_el, beta, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:2]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  day_counter <- 0
  while(day_counter <= days) {

    tmp_infection_status <- infection_status
    for(selected_edge in 1:e) {

      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          tmp_infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

    }

    infection_status <- tmp_infection_status
    day_counter = day_counter+1
    if (sum(infection_status%%2) == 0) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2)))
}


#---------------#
#  Uniform SIR  #
#---------------#

sim_SIR_unif <- function(network_el, beta, gamma, days) {

  n = network_el[1,4]
  e = network_el[1,5]
  cdata <- network_el[,1:2]

  infection_status <- c(rep(1,n))
  index_infected <- sample(1:n, 1)
  infection_status[index_infected] = 2

  max_infected <- 1

  day_counter <- 0
  while(day_counter <= days) {

    tmp_infection_status <- infection_status
    for(selected_edge in 1:e) {

      if (sum(infection_status[cdata[selected_edge,1:2]]) == 3) {
        if (beta >= runif(1,0,1)) {
          tmp_infection_status[cdata[selected_edge,1:2]] = 2
        }
      }

    }

    for (j in which(infection_status %in% 2)) {
      if (gamma >= runif(1,0,1)) {
        tmp_infection_status[j] = 3
      }
    }

    infection_status <- tmp_infection_status
    curr_infected <- sum(infection_status == 2)
    if (curr_infected > max_infected) {
      max_infected <- curr_infected
    }

    day_counter <- sum(day_counter,1)
    if (sum(infection_status%%2) == n) break
  }
  return(c(day_counter-1,sum(infection_status == 1),sum(infection_status == 2),sum(infection_status == 3),max_infected))
}
