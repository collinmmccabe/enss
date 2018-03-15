#' plot_predict
#'
#' @param SI_sim_max_output
#' @param SIR_sim_max_output
#' @param n_min
#'
#' @return
#' @export
#'
#' @examples
plot_predict <- function(SI_sim_max_output, SIR_sim_max_output, n_min) {

  SI_max_output <- data.frame(days = NA, n = NA)
  for(i in 1:length(SI_sim_max_output)) {
    tmp <- data.frame(days = SI_sim_max_output[[i]][1,which(SI_sim_max_output[[i]][2,] == 0)], n = n_min + i - 1)
    SI_max_output <- rbind(SI_max_output, tmp)
  }

  SIR_max_output <- data.frame(days = NA, peak = NA, n = NA)
  for(i in 1:length(SIR_sim_max_output)) {
    tmp <- data.frame(days = SIR_sim_max_output[[i]][1,which(SIR_sim_max_output[[i]][4,] == i)], peak = SIR_sim_max_output[[i]][5,which(SIR_sim_max_output[[i]][4,] == i)], n = n_min + i - 1)
    SIR_max_output <- rbind(SIR_max_output, tmp)
  }

  SImod2 <- predict_SI_max(SI_sim_max_output, n_min)
  SIRmod2 <- predict_SIR_max(SIR_sim_max_output, n_min)

  gg1.rma <- ggplot(SI_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SImod2$regression.results$Slope[4], intercept = SImod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=27, y=1.3, label="y = 0.85 + 0.21 * x\nR-sq. = 0.470", color="black")
  gg1.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SI Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

  gg2.rma <- ggplot(SIR_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SIRmod2$regression.results$Slope[4], intercept = SIRmod2$regression.results$Intercept[4], col="red", size=2)+ annotate(geom="text", x=125, y=1.3, label="y = 0.31 + 0.07 * x\nR-sq. = 0.376", color="black")
  gg2.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SIR Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

  grid.arrange(gg1.rma+ggtitle("(A) SI Model")+theme(legend.position="none", axis.title=element_blank()), gg2.rma+ggtitle("(B) SIR Model")+theme(legend.position="none", axis.title=element_blank()), bottom = "Outbreak duration (days)", left="log [ Network size ]", layout_matrix = matrix(c(1,2), ncol=2, byrow=TRUE))
}
