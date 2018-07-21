################################################################################
#
# Getting geo coordinates
#
################################################################################

rm(list = ls())

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "psych", "maptools", 
                   "rgdal", "rgeos", "lattice", "MASS", "GGally",
                   "ggmap", "ggplot2", "raster", "sp")
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
# Shapefile Source Germany: http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div
# Europe: https://www.naturalearthdata.com/downloads/110m-cultural-vectors/

# Set wd to where shape file is loacated
setwd("/Users/claudiaguenther/Documents/dolores/input/Geodata/vg2500_geo84")

s.ger    <- raster::shapefile("/Users/claudiaguenther/Documents/dolores/input/Geodata/vg2500_geo84/vg2500_bld.shp")
#s.europe <- raster::shapefile("/Users/claudiaguenther/Documents/dolores/input/Geodata/europe/Europe_coastline_shapefile/Europe_coastline_poly.shp")
s.world  <- raster::shapefile("/Users/claudiaguenther/Documents/dolores/input/Geodata/europe/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")

plot(s.ger)
proj4string(s.ger) # WSG84 projection

s.europe <- s.world[s.world@data$CONTINENT == "Europe",]
s.europe <- s.europe[!(s.europe@data$SOVEREIGNT == "Russia"),] #Exclude Russia
s.europe <- s.europe[!(s.europe@data$SOVEREIGNT == "Iceland"),] #Exclude Iceland

rm(s.world)

plot(s.europe)
proj4string(s.europe) # WSG84 projection

make.grid <- function(starting.lon,starting.lat, lat.spacing, lon.spacing, n.lon){
           
    x1    <- seq(from = starting.lon, to = starting.lon + lon.spacing*n.lon, by = lon.spacing)
    n.lat <- n.lon/lon.spacing*lat.spacing
    x2    <- seq(from = starting.lat, to = starting.lat + lat.spacing*n.lat, by = lat.spacing)
    lon <- rep(x1, length(x2))
    lat <- rep(x2, each = length(x1))
    grid      <- data.frame(cbind(lon, lat))
    
        return(grid)
}

grid <- make.grid(starting.lon = 6.1, starting.lat = 47.1, 
                  lat.spacing = 0.5, lon.spacing = 0.625,  n.lon = 30)


grid.europe <- make.grid(starting.lon = -10, starting.lat = 35, 
                  lat.spacing = 1, lon.spacing = 1.25,  n.lon = 100)

# Transform grid to spatial data
grid        <- SpatialPoints(grid, proj4string = CRS(proj4string(s.ger)))
grid.europe <- SpatialPoints(grid.europe, proj4string = CRS(proj4string(s.europe)))

plot(s.ger)
plot(grid, pch = ".", add = T)

plot(s.europe)
plot(grid.europe, pch = ".", add = T)

# Use only grid points within Germany / Europe
grid          <- grid[s.ger, ]
grid.europe   <- grid.europe[s.europe, ]

# Plot again
plot(s.ger, x = )
plot(grid, pch = ".", add = T)

plot(s.europe)
plot(grid.europe, pch = ".", add = T)

# Get coordinates from sites
dat.coord            <- coordinates(grid)
dat.coord.europe     <- coordinates(grid.europe)


# Export coordinates
write.csv(dat.coord, file = "/Users/claudiaguenther/Documents/dolores/input/Geodata/coordinates.csv")
write.csv(dat.coord.europe, file = "/Users/claudiaguenther/Documents/dolores/input/Geodata/coordinates_europe.csv")

