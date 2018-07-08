################################################################################
#
# Download time series from renewables ninja
#
################################################################################

rm(list = ls())

# Change your wd
source('/Users/claudiaguenther/Documents/dolores/input/Code/R/ninja_automator.r')

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

# establish your authorisation
h = new_handle()
handle_setheaders(h, 'Authorization'=paste('Token ', token))

# Read in csv file with coordinates from Germany
dat.coordinates <- read.csv("/Users/claudiaguenther/Documents/dolores/input/Geodata/coordinates.csv")

# Optionally: Sort from north to south, east to west
#dat.coordinates <- dat.coordinates[order(dat.coordinates$Y, dat.coordinates$X, decreasing = TRUE),]

lat = dat.coordinates$lat
lon = dat.coordinates$lon
no.obs = length(lat)

turbine = rep('Vestas+V80+2000', no.obs)

dat.germany.ts = ninja_aggregate_wind(lat, lon, turbine=turbine) # year is define in source file
dat.germany    = rbind(
                      c(NA,as.character(dat.coordinates$Y)),
                      c(NA,as.character(dat.coordinates$X)),
                      dat.germany.ts)

# Calculate average availabilty for data visualization
availability.vec    <- apply(dat.germany.ts[,2:ncol(dat.germany.ts)], 2, mean)

dat.coordinates.add <- cbind(dat.coordinates, availability.vec)

#write.csv(dat.germany, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_germany.csv")
#dat.germany = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany.csv")


## PV

dat.germany.ts.pv = ninja_aggregate_solar(lat = lat, lon = lon) # year is defined in source file
dat.germany       = rbind(
    c(NA,as.character(dat.coordinates$Y)),
    c(NA,as.character(dat.coordinates$X)),
    dat.germany.ts)
