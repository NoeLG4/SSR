###########################################################
## GENERATING N Fst MATRICES THROUGH BOOTSTRAP OVER LOCI ##
###########################################################

## PACKAGES AND DATA
#install.packages(hierfstat)
library(hierfstat)

setwd("my_path") 
dir()
m <- read.csv2("eg.wo_OutGR.csv") # dummy genotyping without outgroup

## explore  the object
## the object must contain: the genotyping & one column called "pop" with the assignment of individuals to populations
head(m) 
pops <- m$pop
length(unique(pops))


## keep only the genotyping
info <- 1 # number of information columns (no genotyping)
m_data <- m[,(info+1):ncol(m)] 
str(m_data)

## bootstraping function
bt <- function(x, boot, nPOP, aggr){ 
  ## boot = number of boostraps
  ## nPOP = number of populations
  ## aggr = vector of integers/factors (assignment of individuals to populations)
  l_out <<- list() ## empty list of matrices
  
  for (j in 1:boot){
    
    resample <- sample(x, replace = TRUE)
    b <- genet.dist(cbind(aggr,resample), diploid = F, method="Fst")
    matboot <- matrix(0, nrow = nPOP, ncol = nPOP) # nxn matrix filled with 0's
    matboot[lower.tri(matboot)] <- b
    matboot <- as.data.frame(matboot)
    
    l_out[[j]] <<- matboot ## list of matrices
  }
}


## establish the number of populations and the repetitions
boot_reps = 10000 # the example output is done with 100
bt(m_data, boot = boot_reps, nPOP = length(unique(pops)), aggr = pops) 

#system.time({bt (m_data, boot=100, nPOP=11, aggr=pops)}) # it does the same as the last line and returns the computing time

l_out[[1]] # the first matrix

## changing into PHYLIP format
## this part is not completely automatized...

## add a first column for each matrix with the population names
## the names must have 10 characters

prenames <- paste("Pop.", unique(pops), sep = "")
prenames
names_phylip <- gsub("\\s", "_", format(prenames, width = 10))
names_phylip


## just adding the column of names to each matrix
m_out <- list()
for (z in 1:boot_reps) {
  m_out[[z]] <- cbind(names_phylip, l_out[[z]])
}

m_out[[1]] # the first matrix

## export to txt 
sink("R_fst.txt")
options(width = 900)
options(digits = 4) 
options(max.print = 1000000) # for more than 30 pops is mandatory
print(m_out, row.names = F, col.names = F, dec = ".") # be careful with the argument "dec" it depends on the configuration of your computer
sink()

## the last step has  not been automatized:
## open txt with excel
##  1. data in columns: spaces+tabs
##  2. select all: text as number/general (do not allow scientific notation) and establish 4 decimals
##  3. replace x* with nothing
##  4. replace phylip_names with the number of pops (11 in this example) 
##  5. delete the first column
##  6. save (txt format)
##  7. take the document to the "exe" phylip folder
##  8. In "neighbor" don't forget to use the lower triangular matrix 

## FIN
