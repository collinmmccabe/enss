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

# --------------

tiff("Figure5.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

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


# ------------

gg4.rma <- ggplot(SIR_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SIRmod2$regression.results$Slope[4], intercept = SIRmod2$regression.results$Intercept[4], col="red", size=2)+ annotate(geom="text", x=125, y=1.3, label="y = 0.31 + 0.07 * x\nR-sq. = 0.376", color="black")
gg4.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SIR Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")

# -----------


gg1.rma <- ggplot(SI_max_output,aes(x=days,y=log(n))) + theme_classic() + geom_bin2d(bins=16) + geom_abline(slope = SImod2$regression.results$Slope[4], intercept = SImod2$regression.results$Intercept[4], col="red", size=2) + annotate(geom="text", x=27, y=1.3, label="y = 0.85 + 0.21 * x\nR-sq. = 0.470", color="black")
gg1.rma + ggtitle("Relationship between log-transformed Network Size and Outbreak Duration\nfrom 77,000 SI Simulations on Maximally-complete Networks, with RMA Trendline") + labs(x="Outbreak duration (days)", y="log [ Network size ]")
