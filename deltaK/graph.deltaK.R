#####################################################################################################################################################
# DELTA-K FIGURE (in Genetic similarities versus morphological resemblance: Unraveling a polyploid complex in a Mediterranean biodiversity hotspot) #
#####################################################################################################################################################
## script for drawing FIG. 2a

setwd("my_path") 
dir()

## libraries and data
library(grid)
require(gridExtra)
library(ggplot2)

dat <- read.table("valores1.txt", header = T)
dat
dat2 <- read.table("valores2.txt", header = F) # just as an example, this one has not header
dat2


## Basic scatter plot

p <- ggplot(dat, aes(x=x, y=y)) +
  geom_point(size=2.5, col="blue") +
  geom_hline(yintercept = 0, col="gray") +
  geom_line(col="blue") +
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  ggtitle("") +
  xlab("") + ylab("") +
  theme_classic(base_size = 15) 

## check
p

## Basic scatter plot (2)

p2 <- ggplot(dat2, aes(x=V1, y=V2)) +
  geom_point(size=2.5, col="blue") +
  geom_hline(yintercept = 0, col="gray") +
  geom_line(col="blue") +
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  ggtitle("") +
  xlab("") + ylab("") +
  theme_classic(base_size = 15)

## check
p2


## put together the two plots and export to tiff

tiff("deltaK.arrange.tiff", width = 11, height = 4.5, units = 'in', res = 300)
grid.arrange(p, p2, ncol = 2, top = textGrob("DeltaK = mean(|L''(K)|) / sd(L(K))", gp = gpar(fontsize=18)))
dev.off()

## FIN