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

# Effective_SI_max <- readRDS("./sims_maximal/Effective_SI_max_unif")
Effective_SI_max <- readRDS("./sims_maximal/max2/Effective_SI_max2")

nmax=80

SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

# simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)

# ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + scale_x_continuous(limits = c(0, 49)) + theme(text=element_text(family="Lato", size=18)) + geom_bin2d(bins=49)  + xlab("Outbreak Duration (days)") + ylab("Group Size")
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
#+ annotate(geom="text", x=4, y=180, label="y = -28.31 + 8.35 * x\nAdj. R-sq. = 0.425", color="black")
#gg1.ols + ggtitle("Relationship between Network Size and Outbreak Duration\nfrom 197,000 SI Simulations on Maximally-complete Networks, with OLS Trendline") + labs(x="Outbreak duration (days)", y="Network size")
##+ theme(panel.background = element_rect(fill = "black"))

SImod2 <- lmodel2(log(n)~days, data=SI_max_output, range.y = "relative", range.x = "relative")
SImod2resid = rep(NA, nrow(SI_max_output))
for (i in 1:nrow(SI_max_output)) {
  SI_max_output$days[i]
}

gg1.rma <- ggplot(SI_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SImod2$regression.results$Slope[4], intercept = SImod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=27, y=1.3, label="y = 0.85 + 0.21 * x\nR-sq. = 0.470", color="black")
# gg1.rma <- ggplot(SI_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SImod2$regression.results$Slope[4], intercept = SImod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=27, y=3, label="y = 5.29 - 0.51 * x\nR-sq. = 0.507", color="black")
gg1.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SI Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

#------------------------------------------------

Effective_SIS_max <- readRDS("Effective_SIS_max2")

SIS_max_output_eq <- data.frame(eq=Effective_SIS_max[[3]][3,which(Effective_SIS_max[[3]][1,]==50)]/6, n=3)
for(i in 4:nmax) {
  tmp <- data.frame(eq=Effective_SIS_max[[i]][3,which(Effective_SIS_max[[i]][1,]==50)]/i, n=i)
  SIS_max_output_eq <- rbind(SIS_max_output_eq, tmp)
}

summary(lm(n ~ eq, data=SIS_max_output_eq))
SISmod2 <- lmodel2(n ~ eq, data=SIS_max_output_eq, range.y = "relative", range.x = "relative")


SIS_max_output_ext <- data.frame(days=Effective_SIS_max[[3]][1,which(Effective_SIS_max[[3]][1,]<50)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SIS_max[[i]][1,which(Effective_SIS_max[[i]][1,]<50)], n=i)
  SIS_max_output_ext <- rbind(SIS_max_output_ext, tmp)
}

sismaxlm <- lm(n ~ days, data=SIS_max_output_ext); summary(sismaxlm)

gg2.ols <- ggplot(SIS_max_output_ext,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=40) + geom_abline(slope = sismaxlm$coefficients[2], intercept = sismaxlm$coefficients[1], col="red", size=2) + annotate(geom="text", x=35, y=180, label="y = 104.69 - 2.57 * x\nAdj. R-sq. = 0.031", color="black")
gg2.ols + ggtitle("Relationship between Network Size and Extinction Time\nfrom 197,000 SIS Simulations on Maximally-complete Networks, with OLS Trendline") + labs(x="Extinction time (days)", y="Network size")

SISmod2 <- lmodel2(log(n)~days, data=SIS_max_output_ext, range.y = "relative", range.x = "relative")

gg2.rma <- ggplot(SIS_max_output_ext,aes(x=days,y=log(n)))+ theme_classic() + geom_bin2d(bins=23) + geom_abline(slope = SISmod2$regression.results$Slope[4], intercept = SISmod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=35, y=4, label="y = 5.41 - 0.60 * x\nR-sq. = 0.106", color="black")
gg2.rma + ggtitle("Relationship between log-transformed Network Size and Extinction Time\nfrom 197,000 SIS Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Extinction time (days)", y="log [ Network size ]")

#------------------------------------------------

Effective_STD_max <- readRDS("Effective_STD_max2")

STD_max_output <- data.frame(days=Effective_STD_max[[3]][1,which(Effective_STD_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_STD_max[[i]][1,which(Effective_STD_max[[i]][2,]==0)], n=i)
  STD_max_output <- rbind(STD_max_output, tmp)
}

stdmaxlm<-lm(n ~ days, data=STD_max_output); summary(stdmaxlm)

gg3.ols <- ggplot(STD_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=40) + geom_abline(slope = stdmaxlm$coefficients[2], intercept = stdmaxlm$coefficients[1], col="red", size=2) + annotate(geom="text", x=75, y=7, label="y = 36.08 + 3.50 * x\nAdj. R-sq. = 0.195", color="black")
gg3.ols + ggtitle("Relationship between Network Size and Outbreak Duration\nfrom 197,000 STD Simulations on Maximally-complete Networks, with OLS Trendline") + labs(x="Outbreak duration (days)", y="Network size")

STDmod2 <- lmodel2(log(n)~days, data=STD_max_output, range.y = "relative", range.x = "relative")

gg3.rma <- ggplot(STD_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=17) + geom_abline(slope = STDmod2$regression.results$Slope[4], intercept = STDmod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=75, y=2, label="y = 0.62 + 0.20 * x\nR-sq. = 0.240", color="black")
gg3.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 93,000 STD Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

#------------------------------------------------

# Effective_SIR_max <- readRDS("./sims_maximal/Effective_SIR_max_unif")
Effective_SIR_max <- readRDS("./sims_maximal/max2/Effective_SIR_max2")

SIR_max_output <- data.frame(days=Effective_SIR_max[[3]][1,which(Effective_SIR_max[[3]][4,]==3)], peak=Effective_SIR_max[[3]][5,which(Effective_SIR_max[[3]][4,]==3)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SIR_max[[i]][1,which(Effective_SIR_max[[i]][4,]==i)], peak=Effective_SIR_max[[i]][5,which(Effective_SIR_max[[i]][4,]==i)], n=i)
  SIR_max_output <- rbind(SIR_max_output, tmp)
}

#sirmaxlm <- lm(n ~ days, data=SIR_max_output); summary(sirmaxlm)

#gg4.ols <- ggplot(SIR_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=40) + geom_abline(slope = sirmaxlm$coefficients[2], intercept = sirmaxlm$coefficients[1], col="red", size=2) + annotate(geom="text", x=150, y=12, label="y = -18.89 + 1.97 * x\nAdj. R-sq. = 0.331", color="black")
#gg4.ols + ggtitle("Relationship between Network Size and Outbreak Duration\nfrom 197,000 SIR Simulations on Maximally-complete Networks, with OLS Trendline") + labs(x="Outbreak duration (days)", y="Network size")

SIRmod2 <- lmodel2(log(n)~days, data=SIR_max_output, range.y = "relative", range.x = "relative")

# gg4.rma <- ggplot(SIR_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SIRmod2$regression.results$Slope[4], intercept = SIRmod2$regression.results$Intercept[4], col="red", size=2)+ annotate(geom="text", x=125, y=1.8, label="y = -0.24 + 0.09 * x\nR-sq. = 0.193", color="black")
gg4.rma <- ggplot(SIR_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SIRmod2$regression.results$Slope[4], intercept = SIRmod2$regression.results$Intercept[4], col="red", size=2)+ annotate(geom="text", x=125, y=1.3, label="y = 0.31 + 0.07 * x\nR-sq. = 0.376", color="black")
gg4.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SIR Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

#------------------------------------------------

#grid.arrange(gg1.ols+ggtitle("A. SI Model")+theme(legend.position="none", axis.title=element_blank()), gg2.ols+ggtitle("B. SIS Model")+theme(legend.position="tion\nfrom Transmission Simulations on Maximally-complete Networks, with OLS Trendlines", gp=gpar(fontsize=16, fontface="bold")), bottom none", axis.title=element_blank()), gg3.ols+ggtitle("C. STD Model")+theme(legend.position="none", axis.title=element_blank()), gg4.ols+ggtitle("D. SIR Model")+theme(legend.position="none", axis.title=element_blank()), top = textGrob("Relationship between Network Size and Outbreak Duration\nfrom Transmission Simulations on Maximally-complete Networks, with OLS Trendlines", gp=gpar(fontsize=16, fontface="bold")), bottom = "Outbreak Duration (Days)", left="Network Size", layout_matrix = matrix(c(1,2,3,4), ncol=2, byrow=TRUE))

tiff("Figure2.tiff", height = 9, width = 18, units = 'cm', compression = "lzw", res = 300)

grid.arrange(gg1.rma+ggtitle("(A) SI Model")+theme(legend.position="none", axis.title=element_blank()), gg4.rma+ggtitle("(B) SIR Model")+theme(legend.position="none", axis.title=element_blank()), bottom = "Outbreak duration (days)", left="log [ Network size ]", layout_matrix = matrix(c(1,2), ncol=2, byrow=TRUE))

dev.off()



#----------------#
#   Comparison   #
#  of ERG v Max  #
#----------------#

SImod2resid = rep(NA, nrow(SI_max_output))
for (i in 1:nrow(SI_max_output)) {
  SImod2resid[i] <- ((SImod2$regression.results$Intercept[4] + SImod2$regression.results$Slope[4]*SI_max_output$days[i])-log(SI_max_output$n[i]))^2
}

efgrsz.SI <- function(x) {
  y = round(exp(SImod2$regression.results$Intercept[4] + SImod2$regression.results$Slope[4]*x + mean(SImod2resid)/2))
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

#hist_si <- hist(Effective_SI_max[[efgrsz.SI(mean(Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)]))]][1,],  xlim=c(6,32), col=rgb(1,0,0,1),  main="A. Representative Comparison of Histograms for Outbreak Times", xlab="Outbreak Time (days)") + hist(Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)], col=rgb(0,0,1,0.5), add=T)

SIlist <- list(Effective_SI_max[[efgrsz.SI(mean(Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)]))]][1,], Effective_SI_erg[[nx]][[px]][[sx]][1,which(Effective_SI_erg[[nx]][[px]][[sx]][2,]==0)])

par(mar=c(4, 2, 2, 0))
multhist(SIlist, xlab = "Outbreak duration (days)", main="(A) Representative comparison of histograms for outbreak durations")

SI_erg_output[which(SI_erg_output$n==50 & SI_erg_output$p==15 & SI_erg_output$samp==7),]

par(mar=c(4,5,2,1))
plot(SI_erg_output$exp_mu[2:1000] ~ SI_erg_output$obs_mu[2:1000], pch=20, bty="l", xlab="Mean outbreak duration on random network", ylab="Mean outbreak duration on\neffective network", main="(B) Mean outbreak duration correlation")
abline(a=0, b=1, col="red")

#viomu_si <- my.vioplot(SI_erg_output$obs_mu[2:1000], SI_erg_output$exp_mu[2:1000], names=c("Observed", "Expected"), col=c("red", "blue"), border=c("red", "blue")) + title(ylab="Mean Outbreak Time", main="C. Distributions of Mean Outbreak Times")

#viosd_si <- my.vioplot(SI_erg_output$obs_sd[2:1000], SI_erg_output$exp_sd[2:1000], names=c("Observed", "Expected"), col=c("red", "blue"), border=c("red", "blue")) + title(ylab="Standard Deviation in Outbreak Time", main="C. Distributions of Standard Deviations in Outbreak Times")

matplot(t(data.frame(SI_erg_output$obs_sd[2:1000],SI_erg_output$exp_sd[2:1000])), type="b", pch=20, col=1, lty=1, bty="l", ylab="Standard deviations", xaxt="n", main="(C) Pairwise comparison of standard deviation")
axis(1, at=c(1,2), labels = c("Random", "Effective"))

t.test(SI_erg_output$obs_mu[2:1000], SI_erg_output$exp_mu[2:1000], paired = T)

#ks_si <- vioplot(SI_erg_output$kst_D[which(SI_erg_output$p==15 & SI_erg_output$n==50)], SI_erg_output$kst_D[which(SI_erg_output$p==25 & SI_erg_output$n==50)], SI_erg_output$kst_D[which(SI_erg_output$p==35 & SI_erg_output$n==50)], names=c("15", "25", "35"), col="gray", border="gray") + title(ylab="Kolmogorov-Smirnov D-Statistic",xlab="Percent possible ties present", main="D. Distributions of Kolmogorov-Smirnov D-statistics comparing Outbreak Times (for n=50)")
#abline(a=0.059, b=0, lty=2)

hist(SI_erg_output$kst_D, xlab="Kolmogorov-Smirnov D-statistic", main="(D) Distributions of Kolmogorov-Smirnov D-statistics", col="gray")

##vioplot(SI_erg_output$kst_p[which(SI_erg_output$n==10)], SI_erg_output$kst_p[which(SI_erg_output$n==30)], SI_erg_output$kst_p[which(SI_erg_output$n==50)], names=c("10", "30", "50"))
##title(ylab="Kolmogorov-Smirnov p-value",xlab="Network size", main="Comparison of Kolmogorov-Smirnov P-values Distributions\nbetween Various Network Sizes for SI Simulations")

#mtext("Comparison of Distributions in Outbreak Times from Observed (ERG; Red)\nand Expected (Maximally-complete; Blue) SI Simulation Results", font=2, side=3, outer=TRUE, line=0)

dev.off()

#------------------------------------------------

SIRmod2resid = rep(NA, nrow(SIR_max_output))
for (i in 1:nrow(SIR_max_output)) {
  SIRmod2resid[i] <- ((SIRmod2$regression.results$Intercept[4] + SIRmod2$regression.results$Slope[4]*SIR_max_output$days[i])-log(SIR_max_output$n[i]))^2
}

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

tiff("figures/Figure5.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

par(oma=c(0, 0, 0, 0))
layout(matrix(c(1,1,2,3,4,4), 3, 2, byrow = TRUE))

nx=30; px=25; sx=39

#hist(Effective_SIR_max[[efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)]))]][1,which(Effective_SIR_max[[efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)]))]][4,]==efgrsz.SIR(mean(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)])))],  xlim=c(0,110) ,col=rgb(0,0,1,0.5),  main="A. Representative Comparison of Histograms for Outbreak Times", xlab="Outbreak Time (days)", breaks=18)
#hist(Effective_SIR_erg[[nx]][[px]][[sx]][1,which(Effective_SIR_erg[[nx]][[px]][[sx]][4,]==nx)], col=rgb(1,0,0,0.5), breaks=18, add=T)

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
