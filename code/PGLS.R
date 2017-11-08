#----------------#
#   Predicting   #
#  EGS from Emp  #
#----------------#

Effective_el_emp <- readRDS("./sims_empirical/Effective_el_emp_w")
Effective_spp_emp <- readRDS("./sims_empirical/Effective_spp_emp_w")
Effective_SI_emp <- readRDS("./sims_empirical/Effective_SI_emp_w")

SI_emp_output <- data.frame(n.exp=efgrsz.SI(mean(Effective_SI_emp[[1]][1,which(Effective_SI_emp[[1]][2,]==0)])), n.obs=Effective_el_emp[[1]][1,4])
for(i in 2:length(Effective_SI_emp)) {
  tmp <- data.frame(n.exp=efgrsz.SI(mean(Effective_SI_emp[[i]][1,which(Effective_SI_emp[[i]][2,]==0)])), n.obs=Effective_el_emp[[i]][1,4])
  SI_emp_output <- rbind(SI_emp_output, tmp)
}

EGS_emp <- cbind(Effective_spp_emp, n_obs=SI_emp_output[,2], n_exp_si=SI_emp_output[,1])

EGS_emp[which(EGS_emp[,4]<81),]

#------------------------------------------------

Effective_SIR_emp <- readRDS("./sims_empirical/Effective_SIR_emp_w")

SIR_emp_output <- data.frame(n.exp=efgrsz.SIR(mean(Effective_SIR_emp[[1]][1,which(Effective_SIR_emp[[1]][4,]==Effective_el_emp[[1]][1,4])])), n.obs=Effective_el_emp[[1]][1,4])
for(i in 2:length(Effective_SI_emp)) {
  tmp <- data.frame(n.exp=efgrsz.SIR(mean(Effective_SIR_emp[[i]][1,which(Effective_SIR_emp[[i]][4,]==Effective_el_emp[[i]][1,4])])), n.obs=Effective_el_emp[[i]][1,4])
  SIR_emp_output <- rbind(SIR_emp_output, tmp)
}

EGS_emp <- cbind(Effective_spp_emp, n_obs=SI_emp_output[,2], n_exp_si=SI_emp_output[,1], n_exp_sir=SIR_emp_output[,1])
write.csv(x = EGS_emp[which(EGS_emp[,4]<81),], file = "EGS_emp.csv")



#----------------------#
#  Testing Usefulness  #
#   of EGS for Group   #
#   Size PGLS Assocs   #
#----------------------#

iterations = 1000

pldata <- read.csv("./data_files/pglsdata5.csv", header = T)
colnames(pldata)[1] <- "spp"
pltree<-read.nexus("./data_files/consensusTree_10kTrees_Primates_Version3.nex")
pltree<-makeLabel(pltree)

pltree2<-drop.tip(pltree,setdiff(pltree$tip.label,pldata$spp))

tiff("Figure6.tiff", height = 10, width = 8.5, units = 'cm', compression = "lzw", res = 300)

par(mar=c(0, 0, 0, 0), ps=10)
plot(pltree2)

dev.off()

# p_total_resid <- scale(residuals.pgls(pgls(log(p_total+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]
# p_close_resid <- scale(residuals.pgls(pgls(log(p_close+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]
# p_nonclose_resid <- scale(residuals.pgls(pgls(log(p_nonclose+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]
# p_helm_resid <- scale(residuals.pgls(pgls(log(p_helm+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]
# p_virus_resid <- scale(residuals.pgls(pgls(log(p_virus+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]
# p_prot_resid <- scale(residuals.pgls(pgls(log(p_prot+1) ~ log(cit_wos+1), data = primate, lambda='ML')))[,1]

# zresids <- cbind(p_total_resid, p_close_resid, p_nonclose_resid, p_helm_resid, p_virus_resid, p_prot_resid)
# write.csv(zresids, "zresids.csv")


# pldata$n_exp_sir <- scale(log(pldata$n_exp_sir))
# pldata$n_exp_si <- scale(log(pldata$n_exp_si))
pldata$n_exp_sir <- scale(log(pldata$old_exp_sir))
pldata$n_exp_si <- scale(log(pldata$old_exp_si))
pldata$n_obs <- scale(log(pldata$n_obs))
pldata$range_km2 <- scale(log(pldata$range_km2))
pldata$mass_g <- scale(log(pldata$mass_g))

row.names(pldata) <- pldata$spp

primate <- comparative.data(phy = pltree, data = pldata, names.col = spp, vcv = TRUE)
summary(obs1<-pgls(scale(log(p_total)) ~ n_obs +scale(log(cit_wos)), data = primate, lambda='ML'))

primate2 <- comparative.data(phy = pltree, data = pldata[which(pldata$p_close > 0),], names.col = spp, vcv = TRUE)
summary(obs2<-pgls(scale(log(p_close)) ~ n_obs +scale(log(cit_wos)), data = primate2, lambda='ML'))

primate3 <- comparative.data(phy = pltree, data = pldata[which(pldata$p_helm > 0),], names.col = spp, vcv = TRUE)
summary(obs3<-pgls(scale(log(p_helm)) ~ n_obs +scale(log(cit_wos)), data = primate3, lambda='ML'))

primate4 <- comparative.data(phy = pltree, data = pldata[which(pldata$p_prot > 0),], names.col = spp, vcv = TRUE)
summary(obs4<-pgls(scale(log(p_prot)) ~ n_obs +scale(log(cit_wos)), data = primate4, lambda='ML'))

primate5 <- comparative.data(phy = pltree, data = pldata[which(pldata$p_virus > 0),], names.col = spp, vcv = TRUE)
summary(obs5<-pgls(scale(log(p_virus)) ~ n_obs +scale(log(cit_wos)), data = primate5, lambda='ML'))

summary(exp_si1<-pgls(scale(log(p_total)) ~ n_exp_si +scale(log(cit_wos)), data = primate, lambda='ML'))
summary(exp_si2<-pgls(scale(log(p_close)) ~ n_exp_si +scale(log(cit_wos)), data = primate2, lambda='ML'))
summary(exp_si3<-pgls(scale(log(p_helm)) ~ n_exp_si +scale(log(cit_wos)), data = primate3, lambda='ML'))
summary(exp_si4<-pgls(scale(log(p_prot)) ~ n_exp_si +scale(log(cit_wos)), data = primate4, lambda='ML'))
summary(exp_si5<-pgls(scale(log(p_virus)) ~ n_exp_si +scale(log(cit_wos)), data = primate5, lambda='ML'))

summary(exp_sir1<-pgls(scale(log(p_total)) ~ n_exp_sir +scale(log(cit_wos)), data = primate, lambda='ML'))
summary(exp_sir2<-pgls(scale(log(p_close)) ~ n_exp_sir +scale(log(cit_wos)), data = primate2, lambda='ML'))
summary(exp_sir3<-pgls(scale(log(p_helm)) ~ n_exp_sir +scale(log(cit_wos)), data = primate3, lambda='ML'))
summary(exp_sir4<-pgls(scale(log(p_prot)) ~ n_exp_sir +scale(log(cit_wos)), data = primate4, lambda='ML'))
summary(exp_sir5<-pgls(scale(log(p_virus)) ~ n_exp_sir +scale(log(cit_wos)), data = primate5, lambda='ML'))

summary(range_km21<-pgls(scale(log(p_total)) ~ range_km2 +scale(log(cit_wos)), data = primate, lambda='ML'))
summary(range_km22<-pgls(scale(log(p_close)) ~ range_km2 +scale(log(cit_wos)), data = primate2, lambda='ML'))
summary(range_km23<-pgls(scale(log(p_helm)) ~ range_km2 +scale(log(cit_wos)), data = primate3, lambda='ML'))
summary(range_km24<-pgls(scale(log(p_prot)) ~ range_km2 +scale(log(cit_wos)), data = primate4, lambda='ML'))
summary(range_km25<-pgls(scale(log(p_virus)) ~ range_km2 +scale(log(cit_wos)), data = primate5, lambda='ML'))

summary(mass_g1<-pgls(scale(log(p_total)) ~ mass_g +scale(log(cit_wos)), data = primate, lambda='ML'))
summary(mass_g2<-pgls(scale(log(p_close)) ~ mass_g +scale(log(cit_wos)), data = primate2, lambda='ML'))
summary(mass_g3<-pgls(scale(log(p_helm)) ~ mass_g +scale(log(cit_wos)), data = primate3, lambda='ML'))
summary(mass_g4<-pgls(scale(log(p_prot)) ~ mass_g +scale(log(cit_wos)), data = primate4, lambda='ML'))
summary(mass_g5<-pgls(scale(log(p_virus)) ~ mass_g +scale(log(cit_wos)), data = primate5, lambda='ML'))

AICc(exp_si1, exp_sir1, obs1, range_km21, mass_g1)
AICc(exp_si2, exp_sir2, obs2, range_km22, mass_g2)
AICc(exp_si3, exp_sir3, obs3, range_km23, mass_g3)
AICc(exp_si4, exp_sir4, obs4, range_km24, mass_g4)
AICc(exp_si5, exp_sir5, obs5, range_km25, mass_g5)


# write(c("iteration","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsir","p_totsir","R2_totsir","adR2_totsir","l_totsir","beta_closi","p_closi","R2_closi","adR2_closi","l_closi","beta_closi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi","beta_totsi","p_totsi","R2_totsi","adR2_totsi","l_totsi"), append=FALSE)
#
# for (i in 1:iterations)
# {
#   subd_data <- ddply(pldata, .(spp), function(x) {x[sample(nrow(x), 1),]})
#
#   row.names(subd_data) <- subd_data$spp
#
#   primate <- comparative.data(phy = pltree, data = pldata, names.col = spp, vcv = TRUE)
#
#   virus_sir <- pgls(p_virus_resid ~ n_exp_sir, data = primate, lambda='ML')
#
#   write(c(i,summary(total_si)$coefficients[2,1],summary(total_si)$coefficients[2,2],summary(model.pgls)$coefficients[2,3],summary(model.pgls)$coefficients[2,4],summary(model.pgls)$r.squared,summary(model.pgls)$adj.r.squared,summary(model.pgls)$param[2]), "output/animals_output-centralization.txt", ncolumns=8, append=TRUE, sep="\t")
# }
