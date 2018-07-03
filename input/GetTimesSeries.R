################################################################################
#
# Download time series from renewables ninja
#
################################################################################

rm(list = ls())

# Change your wd
#source('/Users/claudiaguenther/Documents/dolores/input/R/ninja_automator.r')

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
token = 'd3152690710e11a7ccc40f4953849bda7d003dcf'

# establish your authorisation
h = new_handle()
handle_setheaders(h, 'Authorization'=paste('Token ', token))

# Read in csv file with coordinates from Germany
dat.coordinates <- read.csv('/Users/claudiaguenther/Documents/dolores/input/coordinates_germany_new.csv')

# Sort from north to south, east to west

dat.coordinates <- dat.coordinates[order(dat.coordinates$Y, dat.coordinates$X, decreasing = TRUE),]

lat = dat.coordinates$Y
lon = dat.coordinates$X
no.obs = length(lat)

turbine = rep('Vestas+V80+2000', no.obs)

dat.germany.ts = ninja_aggregate_wind(lat, lon, turbine=turbine) # year is define in source file
dat.germany    = rbind(c(NA,NA,as.character(dat.coordinates$CST_N)),
                      c(NA,NA,as.character(dat.coordinates$Y)),
                      c(NA,NA,as.character(dat.coordinates$X)),
                      dat.germany.ts)


#write.csv(dat.germany, file = "/Users/claudiaguenther/Documents/dolores/input/timeseries_germany.csv")
#dat.germany = read.csv("/Users/claudiaguenther/Documents/dolores/input/timeseries_germany.csv")
