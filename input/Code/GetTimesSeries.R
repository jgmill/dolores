################################################################################
#
# Download time series from renewables ninja
#
################################################################################

rm(list = ls())


## Change path to working directory
wd.path = "C:/Users/Lenovo/Documents/GitHub/dolores/"

# Source automation file
source(paste0(wd.path,"input/Code/R/ninja_automator.r"))

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "psych", "cluster", "distances", 
                   "ecodist", "magrittr", "lattice", "MASS", "foreign",
                   "NbClust", "factoextra", "caret", "ggplot2", "curl")
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
# Change your API (from renewables ninja account)
# insert your API authorisation token here
token = 'efde8da5c66ef97b495ddddf01ef55da4c8a04e6'
#'efde8da5c66ef97b495ddddf01ef55da4c8a04e6'
# 13acaa28b1df03989474169c5e959dc6014382fb

# establish your authorisation
h = new_handle()
handle_setheaders(h, 'Authorization'=paste('Token ', token))


# Read in csv file with coordinates from europe
dat.coordinates        <- read.csv(paste0(wd.path,"input/Geodata/coordinates.csv"))
dat.coordinates.europe <- read.csv(paste0(wd.path,"input/Geodata/coordinates_europe.csv"))

# Optionally: Sort from north to south, east to west
#dat.coordinates <- dat.coordinates[order(dat.coordinates$Y, dat.coordinates$X, decreasing = TRUE),]

################################################################################
# German data

lat = dat.coordinates$lat
lon = dat.coordinates$lon
no.obs = length(lat)

turbine = rep('Vestas+V80+2000', no.obs)

# Get wind data
dat.germany.wind.ts = ninja_aggregate_wind(lat, lon, turbine=turbine, from = '2014-01-01', to='2014-12-31') 
dat.germany.wind    = rbind(
                      c(NA,as.character(dat.coordinates$lat)),
                      c(NA,as.character(dat.coordinates$lon)),
                      dat.germany.wind.ts)


write.csv(dat.germany.wind, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_wind_14.csv")
#dat.germany.wind = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_wind_14.csv")

# Calculate average availabilty for data visualization
availability.vec    <- apply(dat.germany.wind[1:nrow(dat.germany.wind),2:ncol(dat.germany.wind)], 2, as.numeric)
availability.vec    <- apply(availability.vec, 2, sum)/8760
availability.wind   <- as.data.frame(cbind(availability.vec, t(dat.germany.wind[1:2,2:ncol(dat.germany.wind)])))
availability.wind   <- as.data.frame(apply(availability.wind, 2, as.numeric))
colnames(availability.wind) <- c("avail", "lati", "long")

radius <- sqrt(availability.wind$avail/pi)
symbols(availability.wind$long, availability.wind$lati, circles = radius, inches = 0.1, fg = "white", 
        bg = "red", main = "Sized by availability")

avail.plot <- ggplot(availability.wind, aes(x=long,y=lati, colour = avail)) + geom_point(position=position_jitter(w=0.1,h=0), size = 3) 
avail.plot +scale_color_gradient(low="blue", high="red")


## PV

dat.germany.pv.ts = ninja_aggregate_solar(lat = lat, lon = lon, from = '2014-01-01', to='2014-12-31', tilt = 32) # year is defined in source file
dat.germany.pv.ts = dat.germany.pv.ts[-1,]
dat.germany.pv       = rbind(
    c(NA,as.character(dat.coordinates$lat)),
    c(NA,as.character(dat.coordinates$lon)),
    dat.germany.pv.ts)

write.csv(dat.germany.pv, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_pv_14.csv")
#dat.germany.pv = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany_pv_14.csv")

# Calculate average availabilty for data visualization
availability.vec    <- apply(dat.germany.pv[2:nrow(dat.germany.pv),3:ncol(dat.germany.pv)], 2, as.numeric)
availability.vec    <- apply(availability.vec, 2, sum)/8760
availability.pv     <- cbind(availability.vec, t(dat.germany.pv[1:2,3:ncol(dat.germany.pv)]))
availability.pv     <- data.frame(apply(availability.pv, 2, as.numeric))
colnames(availability.pv) <- c("avail", "lati", "long")

radius <- sqrt(availability.pv$avail/pi)
symbols(availability.pv$long, availability.pv$lati, circles = radius, inches = 0.1, fg = "white", 
        bg = "red", main = "Sized by availability")

avail.plot <- ggplot(availability.pv, aes(x=long,y=lati, colour = avail)) + geom_point(position=position_jitter(w=0.1,h=0), size = 3) 
avail.plot + scale_color_gradient(low="brown", high="yellow")

################################################################################
# European data

lat = dat.coordinates.europe$lat
lon = dat.coordinates.europe$lon
no.obs = length(lat)

turbine = rep('Vestas+V80+2000', no.obs)

# Get wind data
dat.europe.wind.ts = ninja_aggregate_wind(lat, lon, turbine=turbine, from = '2014-01-01', to='2014-12-31') 
dat.europe.wind    = rbind(
    c(NA,as.character(dat.coordinates.europe$lat)),
    c(NA,as.character(dat.coordinates.europe$lon)),
    dat.europe.wind.ts)


write.csv(dat.europe.wind, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_wind_14.csv")
write.csv(dat.europe.wind.ts, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_wind_14_raw.csv")

#dat.europe.wind = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_wind_14.csv")

# Calculate average availabilty for data visualization
availability.vec    <- apply(dat.europe.wind[1:nrow(dat.europe.wind),2:ncol(dat.europe.wind)], 2, as.numeric)
availability.vec    <- apply(availability.vec, 2, sum)/8760
availability.wind   <- as.data.frame(cbind(availability.vec, t(dat.europe.wind[1:2,2:ncol(dat.europe.wind)])))
availability.wind   <- as.data.frame(apply(availability.wind, 2, as.numeric))
colnames(availability.wind) <- c("avail", "lati", "long")

radius <- sqrt(availability.wind$avail/pi)
symbols(availability.wind$long, availability.wind$lati, circles = radius, inches = 0.1, fg = "white", 
        bg = "red", main = "Sized by availability")

avail.plot <- ggplot(availability.wind, aes(x=long,y=lati, colour = avail)) + geom_point(position=position_jitter(w=0.1,h=0), size = 3) 
avail.plot +scale_color_gradient(low="grey", high="blue")


## PV

dat.europe.pv.ts = ninja_aggregate_solar(lat = lat, lon = lon, from = '2014-01-01', to='2014-12-31' ) # year is defined in source file
dat.europe.pv.ts = dat.europe.pv.ts[-1,]
dat.europe.pv       = rbind(
    c(NA,as.character(dat.coordinates.europe$lat)),
    c(NA,as.character(dat.coordinates.europe$lon)),
    dat.europe.pv.ts)

write.csv(dat.europe.pv, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_pv_14.csv")
write.csv(dat.europe.pv.ts, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_pv_14_raw.csv")

#dat.europe.pv = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_europe_pv_14.csv")

# Calculate average availabilty for data visualization
availability.vec    <- apply(dat.europe.pv[2:nrow(dat.europe.pv),3:ncol(dat.europe.pv)], 2, as.numeric)
availability.vec    <- apply(availability.vec, 2, sum)/8760
availability.pv     <- cbind(availability.vec, t(dat.europe.pv[1:2,3:ncol(dat.europe.pv)]))
availability.pv     <- data.frame(apply(availability.pv, 2, as.numeric))
colnames(availability.pv) <- c("avail", "lati", "long")

radius <- sqrt(availability.pv$avail/pi)
symbols(availability.pv$long, availability.pv$lati, circles = radius, inches = 0.1, fg = "white", 
        bg = "red", main = "Sized by availability")

avail.plot <- ggplot(availability.pv, aes(x=long,y=lati, colour = avail)) + geom_point(position=position_jitter(w=0.1,h=0), size = 3) 
avail.plot + scale_color_gradient(low="brown", high="yellow")

