###################
## str barplots  ##
###################

# FROM STRUCTURE THROUGH CLUMPP

# libraries ad paths
setwd("my_path")
path_input.CLUMPP <- "my_path_to_CLUMPP_files"


library(ggplot2)
require(gridExtra) # grid.arrange function
library(strataG) # structurePlot function

# select k
k = 3

# data from clumpp (do not modify clumpp file)
lista_outputs <- list.files(path=path_input.CLUMPP, pattern='*.outfile-ind', full.names=F)
lista_outputs 

q.mat <- read.table(paste(path_input.CLUMPP,lista_outputs[1],sep=""), head=F)
nombres <- vector(length=length(q.mat))

for (i in 6:length(q.mat)){nombres[i] <- paste("cluster", i-5, sep="")}
nombres
names(q.mat)[6:length(q.mat)] <- nombres[6:length(nombres)]
k_check = ncol(q.mat)-5
isTRUE(k==k_check) # check point

head(q.mat)

# sort by population (pops as numbers)
q.mat_sort <- q.mat[order(q.mat$V4),] # ascending, -q.mat$V4: descending

# for high number of individuals (three barplots for better visualization)
dim(q.mat)[1]/3 # just for calculate how many inds. in each plot (+- 150 each)
div1 <- 152 # numbers selected for avoiding split populations
div2 <- 296 
no.ind.total <- dim(q.mat)[1] # total number of inds.


# colors
miscolores <- palette(c("darkorchid", "violet", "midnightblue")) # the number of colors depends on the K, you can chose them... 
#miscolores <- heat.colors(k) ## ...or you can use this line

# structurePlot for the barplots 
a_plot2 <- structurePlot(q.mat_sort[1:div1,], pop.col = 4, prob.col = 6, sort.probs = T,
                         label.pops = T, col = miscolores, horiz = F, type = "bar", legend.position = "right")
a_plot2 <- a_plot2 +  
  theme(panel.background = element_rect(fill="white", colour="white"), axis.text.x = element_text(size=10, angle = 90, hjust = 1, vjust = 0.5, colour="black"))
        

b_plot2 <- structurePlot(q.mat_sort[(div1+1):div2,], pop.col = 4, prob.col = 6, sort.probs = T,
                         label.pops = T, col = miscolores, horiz = F, type = "bar", legend.position = "right")
b_plot2 <- b_plot2 +  
  theme(panel.background = element_rect(fill="white", colour="white"), axis.text.x = element_text(size=10, angle = 90, hjust = 1, vjust = 0.5, colour="black")) 
  

c_plot2 <- structurePlot(q.mat_sort[(div2+1):no.ind.total,], pop.col = 4, prob.col = 6, sort.probs = T,
                         label.pops = T, col = miscolores, horiz = F, type = "bar", legend.position = "right")
c_plot2 <- c_plot2 +  
  theme(panel.background = element_rect(fill="white", colour="white"), axis.text.x = element_text(size=10, angle = 90, hjust = 1, vjust = 0.5, colour="black")) 


# put together
str4 <- grid.arrange(a_plot2, b_plot2, c_plot2, nrow = 3)

# export
tiff("str.K3_SortByPop.tiff", width = 11, height = 8, units = 'in', res = 300)
plot(str4)
dev.off()

## FIN