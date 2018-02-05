#-------------#
#  Importing  #
#  Empirical  #
#  Networks   #
#-------------#

emp <- import_emp("./empirical_edgelists/")

emp$spp[,2] <- c("Alouatta_caraya", "Alouatta_guariba", "Ateles_geoffroyi", "Pan_paniscus", "Brachyteles_arachnoides", "Callithrix_jacchus", "Trachypithecus_pileatus", "Cebus_apella", "Cebus_capucinus", "Cercopithecus_aethiops", "Cercopithecus_campbelli", "Cercopithecus_mitis", "Pan_troglodytes", "Colobus_guereza", "Erythrocebus_patas", "Petterus_fulvus", "Macaca_fuscata", "Macaca_fuscata", "Semnopithecus_entellus", "Lemur_catta", "Macaca_arctoides", "Macaca_assamensis", "Macaca_fascicularis", "Macaca_fuscata", "Macaca_mulatta", "Macaca_radiata", "Macaca_tonkeana", "Mandrillus_sphinx", "Miopithecus_talapoin", "Macaca_mulatta", "Pan_paniscus", "Pan_troglodytes", "Papio_hamadryas", "Papio_ursinus", "Papio_papio", "Papio_ursinus", "Nasalis_larvartus", "Saguinus_fuscicollis", "Saguinus_mystax", "Saimiri_sciureus", "Theropithecus_gelada", "Trachypithecus_johnii")

saveRDS(emp$g,file="./data/Effective_g_emp")
saveRDS(emp$el,file="./data/Effective_el_emp")
saveRDS(emp$el_w,file="./data/Effective_el_emp_w")
saveRDS(emp$spp,file="./data/Effective_spp_emp")

manuscript_species <- c(1,3,8,9,10,12,14,16,20,21,22,23,30,26,29,13,34,38,39,41)
