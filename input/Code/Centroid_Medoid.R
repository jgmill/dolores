################################################################################
#
# Extract Centroids or Medoids from clusters
#
################################################################################

rm(list = ls())

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "psych", "cluster", "distances", 
                   "ecodist")
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
################################################################################
# NOTE: Edit this section only

## Change your wd (location of time series files)
wd.path          <- "/Users/Lenovo/Documents/Github/dolores/input/"

# Change path were to export the files to
file.path.export <- "/Users/Lenovo/Documents/Github/dolores/input/"
  
# Give the file names
wind.name <- "timeseries_germany_wind_14.csv"
pv.name   <- "timeseries_germany_pv_14.csv"
  
# Year of data (2 digit) 
year <- 14    
    
## Set the number of clusters (e.g. 2)
no.cluster     <- 6

## Determine wether use want to export centroids (TRUE) or medoids (FALSE)
cluster.export.centroid <- TRUE

################################################################################
################################################################################

# Read in wind and pv data set
dat.germany.wind = read.csv(file = paste0(wd.path, "/", wind.name))
dat.germany.pv   = read.csv(file = paste0(wd.path, "/", pv.name))
colnames(dat.germany.wind)[2]   <- "hour"
colnames(dat.germany.pv)[2]     <- "hour"
rownames(dat.germany.wind)[1:2] <- c("lat", "lon")
rownames(dat.germany.pv)[1:2]   <- c("lat", "lon")

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

# Function to find centroid in cluster i
clust.centroid = function(dat, clusters.IND) {
    
    clusters.found = unique(clusters.IND)
    centroid.list  = numeric()
    
    for(j in 1:length(clusters.found)){
        
        c   = clusters.found[j]
        memb = which(clusters.IND == c)
        
        # Find medoid (shortes distance to other cluster members)
        centroid = colMeans(dat[memb,])
        
        # Save centroid
        if(j == 1){
        centroid.list = centroid 
        
        } else {
        centroid.list = rbind(centroid.list, centroid)}
    }    
  
  rownames(centroid.list)  = clusters.found
  return(centroid.list)
}

# Function to find medoid in clusters i
clust.medoid = function(distancematrix, clusters.IND) {
    
    clusters.found = unique(clusters.IND)
    cluster.list   = list()
    
    for(j in 1:length(clusters.found)){
        
        c   = clusters.found[j]
        memb = which(clusters.IND == c)
        
        # Find medoid (shortes distance to other cluster members)
        cluster.list[[j]] = names(which.min(rowSums(distancematrix[memb, memb])))
        
    }    
    
    unlist(cluster.list)
   
}

# Determine the number of clusters
clusters = cutree(clus, k = no.cluster) 

# Get Ward's centroids: Use for k mean initialization
centroids = clust.centroid(dat.germany.tr, clusters)

# Run k means on extracted centroids
cluster.k          <-  kmeans(dat.germany.tr, centers = centroids)
cluster.centroid   <-  cluster.k$centers

# Get number of units within each cluster
cluster.size <- cluster.k$size
cluster.size <- cbind(cluster.size, 1:no.cluster)

# Oberve change of cluster membership
final.table <- table(cluster.k$cluster, clusters) # relatively stable
final.memb  <- cluster.k$cluster 

# Get medoids
cluster.medoid.IND  = clust.medoid(distancematrix = as.matrix(Dis.ecl), clusters.IND = final.memb)
cluster.medoid.wind = data.frame(t(dat.germany.wind[-c(1:2),paste0("outputV", cluster.medoid.IND)]))
cluster.medoid.pv   = data.frame(t(dat.germany.pv[-c(1:2), paste0("outputV", cluster.medoid.IND)]))

colnames(cluster.medoid.wind) = dat.germany.wind[-c(1:2),]$hour
colnames(cluster.medoid.pv)   = dat.germany.pv[-c(1:2),]$hour

# Get centroids
cluster.centroid.wind = data.frame(cluster.centroid[,1:8760])
cluster.centroid.pv   = data.frame(cluster.centroid[,8761:17520])

colnames(cluster.centroid.wind) = dat.germany.wind[-c(1:2),]$hour
colnames(cluster.centroid.pv)   = dat.germany.pv[-c(1:2),]$hour

# Export medoid/centroids membership to dataframe
if (cluster.export.centroid){
    
    write.csv(cluster.centroid.wind, file = paste0(file.path.export, "/", "wind.centroids", no.cluster, "_", year))
    write.csv(cluster.centroid.pv,   file = paste0(file.path.export, "/", "pv.centroids", no.cluster, "_", year))
    write.csv(cluster.size,          file = paste0(file.path.export, "/", "cluster.size", no.cluster, "_", year))
    
} else {
 
    
    write.csv(cluster.medoid.wind, file = paste0(file.path.export, "/", "wind.medoids", no.cluster, "_", year))
    write.csv(cluster.medoid.pv,   file = paste0(file.path.export, "/", "pv.medoids", no.cluster, "_", year))
    write.csv(cluster.size,          file = paste0(file.path.export, "/", "cluster.size", no.cluster, "_", year))
    
        }

rm(list=ls())