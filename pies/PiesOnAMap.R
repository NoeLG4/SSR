###################
## PIES ON A MAP ##
###################
## script for drawing FIG. 3 in Genetic similarities versus morphological resemblance: Unraveling a polyploid complex in a Mediterranean biodiversity hotspot

# example k = 2

# libraries and paths
library(ggplot2)
library(grid)
library(plyr)
library(mapproj)

setwd("my_path")
path_input.CLUMPP<- ("my_path_to_CLUMPP_files")

# select your K
k = 2

# Custom color scale 
clusterN <- vector(length = k)
for (i in 1:k)  {clusterN[i] <- paste("cluster", i, sep="")}

my_colors <- structure(c("greenyellow", "yellow"), # select the colors or use a predefined palette with k colors like heat.colors(k)
                       .Names = clusterN) 
# no. of colors = no. of clusters
my_colors

# data: CLUMPP output (populations) 
lista_outputs <- list.files(path = path_input.CLUMPP,pattern='*.outfile-pop', full.names=F)
lista_outputs
qmatrix_parcial <- read.table(paste(path_input.CLUMPP,
                                    lista_outputs[1], sep=""), # select the document
                              header=F)


# you can open the txt before loading and remove ":" symbol or use the next line
# qmatrix_parcial$V1 <- gsub('.$', '', qmatrix_parcial$V1)

names(qmatrix_parcial) <- c("pop",clusterN,"Nindiv") # populations as integers
#head(qmatrix_parcial)

# load coordinates
# this document must have lat, long and a third column with the populations as integers
coord <- read.table("coords.txt", header = F)
names(coord) <- c("decimalLatitude", "decimalLongitude", "pop")


# here we are creating a data.frame which contains:
## column pop (k rows per pop)
## k columns of probability of belonging to each cluster
## column with number of individuals per pop  
## columns with coordinate information
Npop = length(unique(qmatrix_parcial$pop))
isTRUE(Npop==nrow(coord)) ## check


columna <- vector(length = k)
pop <- rep(qmatrix_parcial$pop, k)
class <- rep(clusterN, Npop)
prop_plots <- vector(length = length(pop)) ## k columns
for (i in 1:k) {
  prop_plots[(1+(Npop*(i-1))):(Npop*i)] <- qmatrix_parcial[,i+1]
}

# sort by pop
tabla_graph <- as.data.frame(cbind(pop, prop_plots))
tabla_graph <- tabla_graph[order(pop),]

# merge all
tabla_graph_final <- cbind(tabla_graph, class)
head(tabla_graph_final)
tabla_graph_final$class <- as.character(tabla_graph_final$class)
tabla_graph_final <- merge(tabla_graph_final, coord, by= "pop")

str(tabla_graph_final)

# countries list
WB <- c("Montenegro","Bosnia and Herz.","Croatia","Albania")

# limits of your map
maxY = 47
minY = 39
maxX = 22
minX = 13

# Create a blank map
statemap <- ggplot(tabla_graph_final, aes(decimalLongitude, decimalLatitude, fill=class)) +
  geom_tile() +
  borders("world", region=WB, fill="white", xlim = c(minX,maxX), ylim = c(minY,maxY))+
  coord_map() +
  scale_x_continuous(limits=c(minX,maxX), expand=c(0,0), name = 'Longitude') +
  scale_y_continuous(limits=c(minY,maxY), expand=c(0,0), name = 'Latitude') +
  scale_fill_manual(values = my_colors, name = "")

# Create a list of ggplot objects. Each one is the pie chart for each site 
#https://qdrsite.wordpress.com/2016/06/26/pies-on-a-map/

pies <- dlply(tabla_graph_final, .(pop), function(z)
  ggplot(z, aes(x = factor(1), y = prop_plots, fill = class)) +
    geom_bar(stat='identity', width=1) +
    coord_polar(theta='y') + # if you remove this line you will have bars instead of pies
    scale_fill_manual(values = my_colors) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()))


# Use the latitude and longitude maxima and minima from the map to calculate 
# the coordinates of each site location on a scale of 0 to 1, within the map panel.
piecoords <- ddply(tabla_graph_final, .(pop), function(x) with(x, data.frame(
  pop = pop[1],
  x = (decimalLongitude[1]-minX)/(maxX-minX), 
  y = (decimalLatitude[1]-minY)/(maxY-minY)   
)))

# Print the state map.
statemap # if a contry is displayed "folded", change the limits!

# Use a function from the grid package to move into the viewport that contains the plot panel, so that we can plot the individual pies in their correct locations on the map.
downViewport('panel.7-5-7-5') # the name of the grid viewport can be updated;  call: current.vpTree() and look for the panel

# and now print the pies in your map
#  At each iteration, print the ggplot object at the correct location on the viewport.
for (i in 1:Npop) {
  print(pies[[i]], vp=dataViewport(xData=c(minX,maxX), 
                                   yData=c(minY,maxY), clip='off',
                                   xscale = c(minX,maxX), 
                                   yscale=c(minY,maxY), 
                                   x=piecoords$x[i], 
                                   y=piecoords$y[i], 
                                   height=.065, 
                                   width=.065))
}

# STOP

# if you want the individual pies

for (i in 1:Npop){
  pie <- pies[[i]]
  pdf(paste("pie",i,".pdf", sep="")) ## or tiff, png, etc.
  plot(pie)
  dev.off()
}

## FIN