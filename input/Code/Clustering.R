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

## Change your wd
# Read in wind and pv data set
dat.germany.wind = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_wind_14.csv")
dat.germany.pv   = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_pv_14.csv")
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
clusters = cutree(clus, k = 8) 

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
sil <- silhouette(final.memb , Dis.ecl)
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

heatmap(as.matrix(Dis.ecl))

################################################################################
## Check further alternative clustering ways
fviz_nbclust(dat.germany.tr, FUN = hcut, method = "silhouette")
fviz_nbclust(dat.germany.tr, FUN = hcut, method = "wss")


#library("NbClust")
#nb <- NbClust(dat.final, distance = "euclidean", min.nc = 2,
#              max.nc = 10, method = "kmeans")

#library("factoextra")
#fviz_nbclust(nb)

d <- as.matrix(Dis.ecl)
heatmap(d, symm = TRUE, scale = "none")

levelplot(d[1:ncol(d),ncol(d):1])

################################################################################
## Cluster visualization

final.centers$hours <- 1:nrow(final.centers)
xyplot(X1  + X2 ~ hours, final.centers, type = "l")
summary(final.centers)
apply(final.centers, 2, sd)



# Clear all plots
if(!is.null(dev.list())) dev.off()

plot(y=balancing.ex$X1, x=balancing.ex$hours, ylim=c(0,1.1*max(balancing.ex$X2)),
        col='blue', type='l',
         main='Spatial balancing of wind volatility', xlab='hours', ylab='Availability / Load',
         xaxt='n', yaxt='n')
     points(y=balancing.ex$X2, x=balancing.ex$hours, col='darkblue', type='l', lwd=1)
 axis(2, pretty(c(0, 1.1*max(balancing.ex$X2))), col='blue')
par(new=T)
 plot(y=balancing.ex$demand, x=balancing.ex$hours, ylim=c(0,1.1*max(balancing.ex$demand)),
      col='red', type='l',
      main='Spatial balancing of wind volatility', xlab='hours', ylab='Availability / Load',
      xaxt='n', yaxt='n')
axis(4, pretty(c(0, 1.1*max(balancing.ex$demand))), col='red')

################################################################################
## Compare with current wind generation
ger.wind = read.csv("/Users/claudiaguenther/Documents/Studium/MEMS/SS2018/EnergyInformatics/ninja_wind_country_DE_current-merra-2_corrected.csv")
ger.wind.2014 = as.data.frame(t(ger.wind[298035:306794,c(1,4)]))
colnames(ger.wind.2014) = ger.wind.2014[1,]
ger.wind.2014  = ger.wind.2014[2,]
ger.wind.2014  = as.data.frame(apply(ger.wind.2014, 2, as.numeric))

ger.demand.2014 = read.csv("/Users/claudiaguenther/Documents/Studium/MEMS/SS2018/EnergyInformatics/demand_ger_2014.csv")
ger.demand.2014 = as.data.frame(t(apply(ger.demand.2014, 1, as.numeric)))

# Check alignment of reproduce wind infeed -> coefficients represent capacity per cluster
# Compare to final centroids
dat.comp <- as.data.frame((cbind(final.centers, wind = ger.wind.2014[-c(8759:8760),])))

reg.model <- lmMod <- lm(wind ~ X1 + X2 + X3 + X4, data=dat.comp)  
summary(reg.model)

test <- reg.model$coefficients[2]*dat.comp$X1 + reg.model$coefficients[4]*dat.comp$X2 + 
         reg.model$coefficients[4]*dat.comp$X4 + reg.model$coefficients[5]*dat.comp$X4

matching <- data.frame(cbind(ger.wind.2014[1:8758,], test, 1:8758))
xyplot(V1  + test  ~ V3, matching, type = "l")

################################################################################
## Extended balancing example for wind 2014
# Clear all plots
if(!is.null(dev.list())) dev.off()

library(latticeExtra)
balancing.ex <- cbind(final.centers[6840:7008,c(1:3)], demand = t(ger.demand.2014[6840:7008]))
obj1 <- xyplot(X1  + X2  ~ hours, balancing.ex, type = "l")
obj1
obj2 <- xyplot(demand ~ hours, balancing.ex, type = "l")

doubleYScale(obj1, obj2, add.ylab2 = TRUE, style1 = 1, style2 = 2)

plot(y=balancing.ex$X1, x=balancing.ex$hours, ylim=c(0,1.1*max(balancing.ex$X2)),
     col='blue', type='l',
     main='Spatial balancing of wind volatility', xlab='hours', ylab='Availability / Load',
     xaxt='n', yaxt='n')
points(y=balancing.ex$X2, x=balancing.ex$hours, col='darkblue', type='l', lwd=1)
axis(2, pretty(c(0, 1.1*max(balancing.ex$X2))), col='blue')
par(new=T)
plot(y=balancing.ex$demand, x=balancing.ex$hours, ylim=c(0,1.1*max(balancing.ex$demand)),
     col='red', type='l',
     main='Spatial balancing of wind volatility', xlab='hours', ylab='Availability / Load',
     xaxt='n', yaxt='n')
axis(4, pretty(c(0, 1.1*max(balancing.ex$demand))), col='red')
axis(1, labels = TRUE)

