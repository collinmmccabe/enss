Effective_SI_erg <- readRDS("./sims_ERG/Effective_SI_erg")
Effective_SI_max <- readRDS("./sims_maximal/max2/Effective_SI_max2")

tiff("Figure4.tiff", height = 21, width = 18, units = 'cm', compression = "lzw", res = 300)

plot_compare_SI(Effective_SI_erg, Effective_SI_max, max_nmin = 3, n = 30, p = 25, samp = 39)

dev.off()
