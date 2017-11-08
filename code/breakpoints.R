#------------#
# Max Models #
#------------#

Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max025")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[6]][1,which(Effective_SI_max[[6]][2,]==0)], n=6)
for(i in 7:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
SImod2 <- lmodel2(n~days, data=SI_max_output, range.y = "relative", range.x = "relative")

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 30)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg1.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg1.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=38) + coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg1.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.025") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max050")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 30)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg2.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg2.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=39) + coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg2.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.050") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max075")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 20)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg3.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg3.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=24)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg3.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.075") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/max2/Effective_SI_max2")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 17)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg4.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg4.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=19)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg4.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.100") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max125")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 12)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg5.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg5.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=29)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg5.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.125") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max150")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 9)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg6.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg6.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=24)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg6.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.150") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max175")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 8)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg7.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg7.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=23)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg7.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.175") + labs(x="Outbreak duration (days)", y="Network size")


Effective_SI_max <- readRDS("./sims_maximal/variable_beta/Effective_SI_max200")

nmax=120
SI_max_output <- data.frame(days=Effective_SI_max[[3]][1,which(Effective_SI_max[[3]][2,]==0)], n=3)
for(i in 4:nmax) {
  tmp <- data.frame(days=Effective_SI_max[[i]][1,which(Effective_SI_max[[i]][2,]==0)], n=i)
  SI_max_output <- rbind(SI_max_output, tmp)
}

simaxlm<-lm(n ~ days, data=SI_max_output); summary(simaxlm)
my.seg <- segmented(simaxlm, seg.Z = ~ days, psi = 7)
summary(my.seg)
my.seg$psi
slope(my.seg)

# get the fitted data
my.fitted <- fitted(my.seg)
my.model <- data.frame(OutbreakDuration = SI_max_output$days, NetworkSize = my.fitted)

# add the fitted data to the exisiting plot
gg8.ols + geom_line(data = my.model, aes(x = OutbreakDuration, y = NetworkSize), colour = "tomato")

gg8.ols <- ggplot(SI_max_output,aes(x=days,y=n)) + theme_classic() + geom_bin2d(bins=19)+ coord_cartesian(xlim = c(0, 170))
#+ geom_abline(slope = simaxlm$coefficients[2], intercept = simaxlm$coefficients[1], col="red", size=2)
gg8.ols + ggtitle("Relationship btwn Network Size & Outbreak Duration\nfrom 118,000 SI Simulations with Beta = 0.200") + labs(x="Outbreak duration (days)", y="Network size")

tiff("Figure3.tiff", height = 18, width = 36, units = 'cm', compression = "lzw", res = 150)

# theme_set(theme_classic(base_size = 8))

# grid.arrange(gg1.ols+ggtitle("B = 0.025")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg2.ols+ggtitle("B = 0.050")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg3.ols+ggtitle("B = 0.075")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg4.ols+ggtitle("B = 0.100")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg5.ols+ggtitle("B = 0.125")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg6.ols+ggtitle("B = 0.150")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg7.ols+ggtitle("B = 0.175")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), gg8.ols+ggtitle("B = 0.200")+theme(text=element_text(size=10))+theme(legend.position="none", axis.title=element_blank()), bottom = "Outbreak Duration (Days)", left="Network Size", layout_matrix = matrix(c(1,2,3,4,5,6,7,8), ncol=4, byrow=TRUE))

grid.arrange(gg1.ols+ggtitle("B = 0.025")+theme(legend.position="none", axis.title=element_blank()), gg2.ols+ggtitle("B = 0.050")+theme(legend.position="none", axis.title=element_blank()), gg3.ols+ggtitle("B = 0.075")+theme(legend.position="none", axis.title=element_blank()), gg4.ols+ggtitle("B = 0.100")+theme(legend.position="none", axis.title=element_blank()), gg5.ols+ggtitle("B = 0.125")+theme(legend.position="none", axis.title=element_blank()), gg6.ols+ggtitle("B = 0.150")+theme(legend.position="none", axis.title=element_blank()), gg7.ols+ggtitle("B = 0.175")+theme(legend.position="none", axis.title=element_blank()), gg8.ols+ggtitle("B = 0.200")+theme(legend.position="none", axis.title=element_blank()), bottom = "Outbreak duration (days)", left="Network size", layout_matrix = matrix(c(1,2,3,4,5,6,7,8), ncol=4, byrow=TRUE))

dev.off()
