predict_SIR_max <- function(sim_output, n_min) {

  require(lmodel2)

  SIR_max_output <- data.frame(days = NA, peak = NA, n = NA)

  for(i in 1:length(sim_output)) {

    tmp <- data.frame(days = sim_output[[i]][1,which(sim_output[[i]][4,] == i)], peak = sim_output[[i]][5,which(sim_output[[i]][4,] == i)], n = n_min + i - 1)

    SIR_max_output <- rbind(SIR_max_output, tmp)
  }

  SIRmod2 <- lmodel2(log(n) ~ days, data = SIR_max_output, range.y = "relative", range.x = "relative")

  return(SIRmod2)
}
