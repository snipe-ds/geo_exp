### Geo Experiments Environment Setup & Assign Script (no TS Data) ###


### Load Packages
library(GeoexperimentsResearch) #simple geo assignment functions
library(tidyverse)  # data manipulation
library(factoextra) # clustering algorithms & visualization
library(FactoMineR) # stats package


### Input Variables
csv.importpath <- paste0(getwd(), "/geomultivar.csv") # file name of the geo multi-variate data set
csv.exportfn <- paste(Sys.Date(), "geoassign_noTS.csv", sep = " ") # file name of the geo assignment export
nperstratum <- 4 # set the number of geos you would like per stratum / segment in univariate model
k.target <- 5 # configure to target centers or clusters from fviz_nbclust in prelim analysis section


### Load Data
geodata <- read.csv (file = csv.importpath, 
                     colClasses = c(geo="character", var1="numeric", var2="numeric", var3="numeric"))

geodata2 <- read.csv (file = csv.importpath, row.names = 1, 
                      colClasses = c(geo="character", var1="numeric", var2="numeric", var3="numeric"))


### Univariate Stratified Geo Assignment (s.geooutput)
s.geooutput <- Randomize(GeoStrata(Geos(geodata, volume = 'var1'), n.groups = nperstratum)) #stratifies by volume, strata sizes of n.groups, and randomly assigns 1:n.groups to each geo within a strata


### Multivariate Clustering

  # Prelim analysis
pcatable <- PCA(geodata2, scale.unit = TRUE, graph = FALSE) # principal component analysis for variable reduction
view(pcatable[["eig"]]) # variable weight summary from PCA analysis
fviz_nbclust(geodata2, kmeans, method = "wss") # decision clusters variable in scripts below

  # Partitioning cluster assignment (k.geooutput env variable)
k.output <- kmeans(geodata2, centers=k.target, nstart=25) # configure centers to target clusters from fviz_nbclust
k.geoassign <- as.data.frame(k.output[["cluster"]])
names(k.geoassign)[1] <- "cluster"
k.geooutput <- merge(rownames_to_column(k.geoassign),rownames_to_column(geodata2)) #combine original dataset and custer assignments
fviz_cluster(k.output, data = geodata2) # two dimension visual of spacial clusters of the top two principal variables

  # Hierarchical cluster assignment (h.geooutput env variable)
h.output <- hcut(geodata2, k=k.target, stand=TRUE) # configure clusters(k) to target clusters from fviz_nbclust
h.geoassign <- as.data.frame(h.output[["cluster"]])
names(h.geoassign)[1] <- "cluster"
fviz_dend(h.output, rect = TRUE, cex = .7, horiz = TRUE) # dendrogram of geo clusters and relative distance
h.geooutput <- merge(rownames_to_column(h.geoassign),rownames_to_column(geodata2)) #combine original dataset and custer assignments
k.h.geooutput <- merge(k.geooutput, h.geooutput, by.x = "rowname", by.y = "rowname") # compare partitioning and hierarchical clustering assignments by geo

### Export Results
write_csv(h.geooutput, file = csv.exportfn) # Export csv in the working direct, x= (s.geooutput or k.geooutput or h.geooutput)

### Other
#get_clust_tendency(geodata2, n=5) # hopkins stat close to 1 indicates clusterability
#fviz_dist(get_dist(geodata2, stand = TRUE)) # visualize geo distance relationship

