#####################################
#  Analyze & Predict Eff Net Size   #
#               - - -               #
#      Compare spread times of      #
#  maximally-complete, Erdos-Renyi, #
#  & empirical graphs (w PGLS)      #
#               - - -               #
#        CM McCabe & CL Nunn        #
#####################################


#------------#
# Max Models #
#------------#

Effective_SI_max <- readRDS("./sims_maximal/max2/Effective_SI_max2")

nmax=80

SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

SImod2 <- lmodel2(log(n)~days, data=SI_max_output, range.y = "relative", range.x = "relative")
SImod2resid = rep(NA, nrow(SI_max_output))
for (i in 1:nrow(SI_max_output)) {
  SI_max_output$days[i]
}

gg1.rma <- ggplot(SI_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SImod2$regression.results$Slope[4], intercept = SImod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=27, y=1.3, label="y = 0.85 + 0.21 * x\nR-sq. = 0.470", color="black")
gg1.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SI Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

#------------------------------------------------

Effective_SIR_max <- readRDS("./sims_maximal/max2/Effective_SIR_max2")

SIR_max_output <- data.frame(days=Effective_SIR_max[[3]][1,which(Effective_SIR_max[[3]][4,]==3)], peak=Effective_SIR_max[[3]][5,which(Effective_SIR_max[[3]][4,]==3)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SIR_max[[i]][1,which(Effective_SIR_max[[i]][4,]==i)], peak=Effective_SIR_max[[i]][5,which(Effective_SIR_max[[i]][4,]==i)], n=i)
  SIR_max_output <- rbind(SIR_max_output, tmp)
}

SIRmod2 <- lmodel2(log(n)~days, data=SIR_max_output, range.y = "relative", range.x = "relative")

gg4.rma <- ggplot(SIR_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SIRmod2$regression.results$Slope[4], intercept = SIRmod2$regression.results$Intercept[4], col="red", size=2)+ annotate(geom="text", x=125, y=1.3, label="y = 0.31 + 0.07 * x\nR-sq. = 0.376", color="black")
gg4.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SIR Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

#------------------------------------------------

tiff("Figure2.tiff", height = 9, width = 18, units = 'cm', compression = "lzw", res = 300)

grid.arrange(gg1.rma+ggtitle("(A) SI Model")+theme(legend.position="none", axis.title=element_blank()), gg4.rma+ggtitle("(B) SIR Model")+theme(legend.position="none", axis.title=element_blank()), bottom = "Outbreak duration (days)", left="log [ Network size ]", layout_matrix = matrix(c(1,2), ncol=2, byrow=TRUE))

dev.off()
