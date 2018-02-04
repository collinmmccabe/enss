Effective_SIR_erg <- readRDS("./sims_ERG/Effective_SIR_erg")
Effective_SIR_max <- readRDS("./sims_maximal/max2/Effective_SIR_max2")

tiff("Figure5.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

plot_compare_SIR(Effective_SIR_erg, Effective_SIR_max, max_nmin = 3, n = 30, p = 25, samp = 39)

dev.off()
