rm(list=ls())
data("TCGA.BRCA")

result = DiffNet.FDR(TCGA.BRCA$X,TCGA.BRCA$group, alpha = 0.2, test.type = "pcor")
Diff.net.connected = result$Diff.net.connected
# Visualize the estimated differential network in an interactive manner.
tkid <- tkplot(Diff.net.connected, vertex.size= degree(Diff.net.connected)*1.2, layout =layout_with_fr,
               vertex.color="red", vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")
# Visualize the estimated differential network in a non-interactive manner.
# grab the coordinates from tkplot
loc <- tkplot.getcoords(tkid)
plot(Diff.net.connected, layout=loc, vertex.size= degree(Diff.net.connected)*1.2, vertex.color="red",
     vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")


rm(list=ls())
data("TCGA.BRCA")
result = DiffNet.FDR(TCGA.BRCA$X,TCGA.BRCA$group, alpha = 0.2, test.type = "pmat")
Diff.net.connected = result$Diff.net.connected
# Visualize the estimated differential network in an interactive manner.
tkid <- tkplot(Diff.net.connected, vertex.size= degree(Diff.net.connected)*1.2, layout =layout_with_fr,
               vertex.color="red", vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")
# Visualize the estimated differential network in a non-interactive manner.
# grab the coordinates from tkplot
loc <- tkplot.getcoords(tkid)
plot(Diff.net.connected, layout=loc, vertex.size= degree(Diff.net.connected)*1.2, vertex.color="red",
     vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")



rm(list=ls())
data("GSE13159.AML")

result = DiffNet.FDR(GSE13159.AML$X,GSE13159.AML$group, alpha = 0.2, test.type = "pcor")
Diff.net.connected = result$Diff.net.connected
# Visualize the estimated differential network in an interactive manner.
tkid <- tkplot(Diff.net.connected, vertex.size= degree(Diff.net.connected)*2, layout =layout_with_fr,
               vertex.color="red", vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")
# Visualize the estimated differential network in a non-interactive manner.
# grab the coordinates from tkplot
loc <- tkplot.getcoords(tkid)
plot(Diff.net.connected, layout=loc, vertex.size= degree(Diff.net.connected)*2, vertex.color="red",
     vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")


rm(list=ls())
data("GSE13159.AML")
result = DiffNet.FDR(GSE13159.AML$X,GSE13159.AML$group, alpha = 0.2, test.type = "pmat")
Diff.net.connected = result$Diff.net.connected
# Visualize the estimated differential network in an interactive manner.
tkid <- tkplot(Diff.net.connected, vertex.size= degree(Diff.net.connected)*2, layout =layout_with_fr,
               vertex.color="red", vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")
# Visualize the estimated differential network in a non-interactive manner.
# grab the coordinates from tkplot
loc <- tkplot.getcoords(tkid)
plot(Diff.net.connected, layout=loc, vertex.size= degree(Diff.net.connected)*2, vertex.color="red",
     vertex.group.cex=0.8, edge.width =1.2, edge.color="orange")




