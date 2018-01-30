#------------#
# Generating #
#    ERGs    #
#------------#

gnm <- gen_erg(samples = 111, nodes = c(10, 30, 50), percents = c(0.15, 0.25, 0.35))

saveRDS(gnm$g,file="Effective_g_erg")
saveRDS(gnm$el,file="Effective_el_erg")
save.image("Effective_Workspace.RData")
rm(gnm_g, gnm_el, p_gnm_g, p_gnm_el, x, g, el, i, m)
