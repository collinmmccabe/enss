cdata_tnet <- as.tnet(cdata, type="weighted one-mode tnet")
rg_reshuffling_w(cdata_tnet, option="links", directed=0)
tnet_igraph(cdata_tnet, type="weighted one-mode tnet", directed=0)
