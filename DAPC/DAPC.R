##################################################################################################################################################
# DAPC FIGURE (in Genetic similarities versus morphological resemblance: Unraveling a polyploid complex in a Mediterranean biodiversity hotspot) #
##################################################################################################################################################
## script for drawing FIG. 4

# libraries and data
library("adegenet")
load("genind_obj.RData")

DUMMYobj # explore

# check pops
unique(pop(DUMMYobj))

# graph
# https://www.rdocumentation.org/packages/adegenet/versions/2.0.1/topics/dapc%20graphics

miscolores <- palette(c("darkred", "red", "darkorange", rep("grey80", 3)))
misformas  <-c(16,17,16,16,15,17) 

dapc.WBalkan.DUMMY <- dapc(DUMMYobj, var.contrib = TRUE, scale = FALSE, n.pca = NULL, n.da = nPop(DUMMYobjTAX)-1)
# 110 PCs retained: this number is obtained through the xvalDapc function


#tiff("DAPC.tiff", width = 8.5, height = 6, units = 'in', res = 300)
scatter(dapc.WBalkan.DUMMY, cell=2, mstree=F, lty=2,lwd=1.5, cex=1, pch=misformas, solid=1, cstar=1, posi.pca="topleft", posi.da="topleft", scree.pca=TRUE,
        inset.pca=c(.01,.3), posi.leg = "bottomright", label="", axesel=FALSE, col=miscolores, leg=T, cleg=0.5)
#dev.off()

## FIN
