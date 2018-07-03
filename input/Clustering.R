################################################################################
#
# Clustering of wind and pv time series
#
################################################################################

rm(list = ls())

# Change your wd
dat.germany = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany.csv")

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "psych", "cluster", "distances", 
                   "ecodist", "magrittr", "lattice", "MASS", "GGally",
                   "NbClust", "factoextra", "caret", "ggplot2")
allPackages    = c(neededPackages %in% installed.packages()[,"Package"])

# Install packages (if not already installed)
if (!all(allPackages)) {
    missingIDX = which(allPackages == FALSE)
    needed     = neededPackages[missingIDX]
    lapply(needed, install.packages)
}

# Load all defined packages
lapply(neededPackages, function(x) suppressPackageStartupMessages(
    library(x, character.only = TRUE)))

################################################################################

# Reshape data (each hour becomes a variable)
dat.germany.tr           = t(dat.germany)
colnames(dat.germany.tr) = dat.germany.tr[3,]
dat.germany.tr           = as.data.frame(apply(dat.germany.tr[-c(1:3),-c(1:3)], 2, as.numeric))
dat                      = scale(dat.germany.tr)

# Use euclidean distance
Dis.ecl <- dist(dat.germany.tr, method = "euclidean")
# levelplot(as.matrix(Dis.ecl)) # takes very long

# Hierachcal cluster analysis based on Ward & euclidean distance
clus <- hclust(Dis.ecl, method = "ward.D2")
plot(clus)
plot(clus, hang = -1, cex = 0.6)
dendrogram <- as.dendrogram(clus)
plot(dendrogram, ylab = "Height", leaflab = "none")

# Function to find medoid in cluster i
clust.centroid = function(i, dat, clusters.IND) {
    ind = (clusters.IND == i)
    colMeans(dat[ind,])
}

clusters = cutree(clus, k = 2) 

# Get centroids: Use for k mean initialization
centroids = t(sapply(unique(clusters), clust.centroid, dat.germany.tr, clusters))

# Run k means on extracted centroids
cluster.k          <-  kmeans(dat.germany.tr, centers = centroids)
cluster.center     <-  cluster.k$centers
# What are unstandardized values ?
final.centers     <- t(sapply(unique(clusters), clust.centroid, dat.germany.tr, cluster.k$cluster))

# Check change of cluster membership
final.table <- table(cluster.k$cluster, clusters) # relatively stable
final.memb  <- cluster.k$cluster 

# Silouette plot: Check suitability of clustering
sil <- silhouette(final.memb , Dis.ecl)
plot(sil, col=1:2, border=NA)


################################################################################
# Cluster visualization

heatmap(as.matrix(Dis.ecl))

################################################################################
## Check further alternative clustering ways
fviz_nbclust(dat.germany.tr, FUN = hcut, method = "silhouette")

#library("NbClust")
#nb <- NbClust(dat.final, distance = "euclidean", min.nc = 2,
#              max.nc = 10, method = "kmeans")

#library("factoextra")
#fviz_nbclust(nb)

d <- as.matrix(Dis.ecl)
heatmap(d, symm = TRUE, scale = "none")

levelplot(d[1:ncol(d),ncol(d):1])