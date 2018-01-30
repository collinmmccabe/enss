#---------------------#
#     Generating      #
# Maximally-Complete  #
#      Networks       #
#---------------------#

max <- gen_max(2, 300)

saveRDS(max$g, file = "Effective_g_max")
saveRDS(max$el, file = "Effective_el_max")
save.image("Effective_Workspace.RData")
rm(max)
