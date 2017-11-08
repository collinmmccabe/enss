###############
#  Empirical  #
#   Primate   #
#   Networks  #
###############

#-----------#
# Importing #
# Empirical #
#  Networks #
#-----------#

# Bring in real networks from primate work
# Simulate disease spread on them to get estimates of effective network size
# run richness PGLS models with effective network size vs group size
# see if effective net size fits better

setwd("empirical_edgelists/")
input_names = list.files()

emp_g = list(NA); emp_el = list(NA); spp_list = data.frame(filename=NA, spp=NA)
for(i in 1:length(input_names)) {
  cat('\n', 'File name:', input_names[i])
  tmp <- read.csv(input_names[i], header = F)

  emp_g[[i]] <- graph.edgelist(as.matrix(tmp[,1:2]), directed=FALSE)
  emp_el[[i]] <- cbind(unique(get.edgelist(emp_g[[i]])), weight=1, n_nodes=c(vcount(emp_g[[i]]), rep(NA, ecount(emp_g[[i]])/2-1)), n_edges=c(ecount(emp_g[[i]])/2, rep(NA, ecount(emp_g[[i]])/2-1)))

  spp_list[i,] <- c(input_names[i], NA)

  ##  plot(emp_g[[i]])
  cat('\n', 'Correctly calculated edge count:', nrow(emp_el[[i]])==emp_el[[i]][1,5], '\n')
}

spp_list[,2] <- c("Alouatta_caraya", "Alouatta_guariba", "Ateles_geoffroyi", "Pan_paniscus", "Brachyteles_arachnoides", "Callithrix_jacchus", "Trachypithecus_pileatus", "Cebus_apella", "Cebus_capucinus", "Cercopithecus_aethiops", "Cercopithecus_campbelli", "Cercopithecus_mitis", "Pan_troglodytes", "Colobus_guereza", "Erythrocebus_patas", "Petterus_fulvus", "Macaca_fuscata", "Macaca_fuscata", "Semnopithecus_entellus", "Lemur_catta", "Macaca_arctoides", "Macaca_assamensis", "Macaca_fascicularis", "Macaca_fuscata", "Macaca_mulatta", "Macaca_radiata", "Macaca_tonkeana", "Mandrillus_sphinx", "Miopithecus_talapoin", "Macaca_mulatta", "Pan_paniscus", "Pan_troglodytes", "Papio_hamadryas", "Papio_ursinus", "Papio_papio", "Papio_ursinus", "Nasalis_larvartus", "Saguinus_fuscicollis", "Saguinus_mystax", "Saimiri_sciureus", "Theropithecus_gelada", "Trachypithecus_johnii")

switch(Sys.info()[['sysname']],
       Windows = {setwd("D:/Dropbox/Effective_Modeling/R/")},
       Linux = {setwd("/media/collin/SSD/Dropbox/Effective_Modeling/R/")},
       Darwin = {setwd("~/Dropbox/Effective_Modeling/R/")})

saveRDS(emp_g,file="Effective_g_emp")
saveRDS(emp_el,file="Effective_el_emp")
saveRDS(spp_list,file="Effective_spp_emp")
rm(emp_g, emp_el, input_names, i, tmp)
