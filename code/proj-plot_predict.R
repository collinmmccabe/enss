Effective_SI_max <- readRDS("./sims_maximal/max2/Effective_SI_max2")
Effective_SIR_max <- readRDS("./sims_maximal/max2/Effective_SIR_max2")

tiff("Figure2.tiff", height = 9, width = 18, units = 'cm', compression = "lzw", res = 300)

plot_predict(Effective_SI_max, Effective_SIR_max, n_min = 3)

dev.off()
