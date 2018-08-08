################################################################################
#
# Clustering of wind and pv time series
#
################################################################################

rm(list = ls())

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

## Change path to working directory
#wd.path = "C:/Users/Lenovo/Documents/GitHub/dolores/"
wd.path = "/Users/claudiaguenther/Documents/dolores/"


# Read in wind and pv data set
dat.germany.wind = read.csv(paste0(wd.path,"input/timeseries_germany_wind_14.csv"))
dat.germany.pv   =read.csv(paste0(wd.path, "input/timeseries_germany_pv_14.csv"))
colnames(dat.germany.wind)[2]   <- "hour"
colnames(dat.germany.pv)[2]     <- "hour"
rownames(dat.germany.wind)[1:2] <- c("lat", "lon")
rownames(dat.germany.pv)[1:2]   <- c("lat", "lon")

################################################################################

# Bind pv and wind data together
dat.germany <- rbind(dat.germany.wind, dat.germany.pv[-c(1:2),])

# Reshape data (each hour becomes a variable)
dat.germany.tre           = as.data.frame(t(dat.germany))
colnames(dat.germany.tre) = unlist(dat.germany.tre[2,])
colnames(dat.germany.tre)[1:2] = c("lat", "lon")
dat.germany.tr           = as.data.frame(apply(dat.germany.tre[-c(1:2),-c(1:2)], 2, as.numeric))

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

# Determine the number of clusters
clusters = cutree(clus, k = 6) 
rect.hclust(clus, k=6, border="red") 


# Get centroids: Use for k mean initialization
centroids = t(sapply(unique(clusters), clust.centroid, dat.germany.tr, clusters))

# Run k means on extracted centroids
cluster.k          <-  kmeans(dat.germany.tr, centers = centroids)
cluster.center     <-  cluster.k$centers

# What are unstandardized values ?
final.centers     <- data.frame((sapply(unique(clusters), 
                                         clust.centroid, 
                                         dat.germany.tr, 
                                         cluster.k$cluster)))

# Oberve change of cluster membership
final.table <- table(cluster.k$cluster, clusters) # relatively stable
final.memb  <- cluster.k$cluster 

# Add cluster membership to dataframe
dat.germany.wind <- data.frame(rbind(c(0, NA, final.memb), dat.germany.wind))
dat.germany.pv   <- data.frame(rbind(c(0, NA, final.memb), dat.germany.pv))

################################################################################
# Check suitability of cluster solution

# Silouette plot: Check suitability of clustering
sil <- cluster::silhouette(final.memb, dist =  Dis.ecl)
plot(sil, col=1:2, border=NA)

################################################################################
# Cluster visualization

# See regional alignment of clusters
check <- cbind(cluster = c(final.memb), dat.germany.tre[-c(1:2),])
#check <- check[order(check$lat, check$lon),]
check$lat <- as.numeric(as.character(check$lat))
check$lon <- as.numeric(as.character(check$lon))
check$cluster <- as.numeric((check$cluster))

scatter.hist(check$cluster, check$lat) 
scatter.hist(check$cluster, check$lon)  

ggplot(check[-1,]) + geom_point(aes(x=lon, y=lat, colour=as.factor(cluster)))


clheatmap(as.matrix(Dis.ecl))

################################################################################
# Add cluster descriptive statistics

mean.avail.vec   <- percent(apply(cluster.k$centers, 1, mean))
mean.av.wind.vec <- percent(apply(cluster.k$centers[,1:8760], 1, mean))
mean.av.pv.vec   <- percent(apply(cluster.k$centers[,8761:17520], 1, mean))
sd.vec           <- apply(cluster.k$centers, 1, sd)
size.vec         <- cluster.k$size

################################################################################




