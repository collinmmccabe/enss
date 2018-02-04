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
# SImod2resid = rep(NA, nrow(SI_max_output))
# for (i in 1:nrow(SI_max_output)) {
#   TODO
# }

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



#----------------#
#   Comparison   #
#  of ERG v Max  #
#----------------#

efgrsz.SI <- function(x) {
  y = round(exp(SImod2$regression.results$Intercept[4] + SImod2$regression.results$Slope[4]*x))
  return(y)
}

Effective_SI_erg <- readRDS("./sims_ERG/Effective_SI_erg")

SI_erg_output <- data.frame(obs_mu=NA, obs_sd=NA, exp_mu=NA, exp_sd=NA, kst_D=NA, kst_p=NA, n=NA, p=NA, samp=NA)
for (n in c(10, 30, 50)) {
  for (p in c(15, 25, 35)) {
    for(i in 1:111) {
      tmp.obs <- Effective_SI_erg[[n]][[p]][[i]][1,which(Effective_SI_erg[[n]][[p]][[i]][2,]==0)]
      tmp.exp <- Effective_SI_max[[efgrsz.SI(mean(tmp.obs))]][1,]
      tmp.kst <- ks.test(jitter(tmp.obs), jitter(tmp.exp))
      tmp <- data.frame(obs_mu=mean(tmp.obs), obs_sd=sd(tmp.obs), exp_mu=mean(tmp.exp), exp_sd=sd(tmp.exp), kst_D=tmp.kst$statistic, kst_p=tmp.kst$p.value, n=n, p=p, samp=i)
      SI_erg_output <- rbind(SI_erg_output, tmp)
    }
    cat("\n", "nodes: ", n, "// percent: ", p)
  }
}

tiff("Figure4.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

par(oma=c(0, 0, 0, 0))
layout(matrix(c(1,1,2,3,4,4), 3, 2, byrow = TRUE))

nx=30; px=25; sx=39

SIlist <- list(Effective_SI_max[[efgrsz.SI(mean(Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)]))]][1,], Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)])

par(mar=c(4, 2, 2, 0))
multhist(SIlist, xlab = "Outbreak duration (days)", main="(A) Representative comparison of histograms for outbreak durations")

SI_erg_output[which(SI_erg_output$n==50 & SI_erg_output$p==15 & SI_erg_output$samp==7),]

par(mar=c(4,5,2,1))
plot(SI_erg_output$exp_mu[2:1000] ~ SI_erg_output$obs_mu[2:1000], pch=20, bty="l", xlab="Mean outbreak duration on random network", ylab="Mean outbreak duration on\neffective network", main="(B) Mean outbreak duration correlation")
abline(a=0, b=1, col="red")

matplot(t(data.frame(SI_erg_output$obs_sd[2:1000],SI_erg_output$exp_sd[2:1000])), type="b", pch=20, col=1, lty=1, bty="l", ylab="Standard deviations", xaxt="n", main="(C) Pairwise comparison of standard deviation")
axis(1, at=c(1,2), labels = c("Random", "Effective"))

t.test(SI_erg_output$obs_mu[2:1000], SI_erg_output$exp_mu[2:1000], paired = T)

hist(SI_erg_output$kst_D, xlab="Kolmogorov-Smirnov D-statistic", main="(D) Distributions of Kolmogorov-Smirnov D-statistics", col="gray")

dev.off()

#------------------------------------------------

efgrsz.SIR <- function(x) {
  y = round(exp(SIRmod2$regression.results$Intercept[4] + SIRmod2$regression.results$Slope[4]*x))
  return(y)
}

Effective_SIR_erg <- readRDS("./sims_ERG/Effective_SIR_erg")

SIR_erg_output <- data.frame(obs_mu=NA, obs_sd=NA, exp_mu=NA, exp_sd=NA, kst_D=NA, kst_p=NA, n=NA, p=NA, samp=NA)
for (n in c(10, 30, 50)) {
  for (p in c(15, 25, 35)) {
    for(i in 1:111) {
      tmp.obs <- Effective_SIR_erg[[n]][[p]][[i]][1,which(Effective_SIR_erg[[n]][[p]][[i]][4,]==n)]
      tmp.exp <- Effective_SIR_max[[efgrsz.SIR(mean(tmp.obs))]][1,]
      tmp.kst <- ks.test(jitter(tmp.obs), jitter(tmp.exp))
      tmp <- data.frame(obs_mu=mean(tmp.obs), obs_sd=sd(tmp.obs), exp_mu=mean(tmp.exp), exp_sd=sd(tmp.exp), kst_D=tmp.kst$statistic, kst_p=tmp.kst$p.value, n=n, p=p, samp=i)
      SIR_erg_output <- rbind(SIR_erg_output, tmp)
    }
    cat("\n", "nodes: ", n, "// percent: ", p)
  }
}

tiff("Figure5.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

par(oma=c(0, 0, 0, 0))
layout(matrix(c(1,1,2,3,4,4), 3, 2, byrow = TRUE))

nx=30; px=25; sx=39

SIRlist <- list(Effective_SIR_max[[efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)]))]][1,which(Effective_SIR_max[[efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)]))]][4,]==efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)])))], Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)])

par(mar=c(4, 2, 2, 0))
multhist(SIRlist, xlab = "Outbreak duration (days)", main="(A) Representative comparison of histograms for outbreak durations")

par(mar=c(4,5,2,1))
plot(SIR_erg_output$exp_mu[2:1000] ~ SIR_erg_output$obs_mu[2:1000], pch=20, bty="l", xlab="Mean outbreak duration on random network", ylab="Mean outbreak duration on\neffective network", main="(B) Mean outbreak duration correlation")
abline(a=0, b=1, col="red")

matplot(t(data.frame(SIR_erg_output$obs_sd[2:1000],SIR_erg_output$exp_sd[2:1000])), type="b", pch=20, col=1, lty=1, bty="l", ylab="Standard deviations", xaxt="n", main="(C) Pairwise comparison of standard deviation")
axis(1, at=c(1,2), labels = c("Random", "Effective"))

hist(SIR_erg_output$kst_D, xlab="Kolmogorov-Smirnov D-statistic", main="(D) Distributions of Kolmogorov-Smirnov D-statistics", col="gray")

dev.off()
