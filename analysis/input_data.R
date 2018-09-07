################################################################################
#
# Analyse input data
#
################################################################################

rm(list = ls())

# List all packages needed for session
neededPackages = c("dplyr", "tidyr", "foreign")
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
# Load data

## Change your wd (location of time series files)
wd.path          <- "/Users/Lenovo/Documents/Github/dolores/model/data/"
# Give the file names
dat.name <- "Germany_cap_from_lit_6G_upload.csv"

dat.input <- read.csv(paste0(wd.path,dat.name), header = TRUE, stringsAsFactors = FALSE)

################################################################################
# Identify peak hours

plot(sort(dat.input[3:8762,]$d.2, decreasing = TRUE))
boxplot(as.numeric(dat.input[3:8762,]$d.2))
mean(as.numeric(dat.input[3:8762,]$d.2))
quantile(as.numeric(dat.input[3:8762,]$d.2))
q75 <- quantile(as.numeric(dat.input[3:8762,]$d.2), probs = 0.75)

peak.vec <- which(as.numeric(dat.input[3:8762,]$d.2) >= q75) + 2

dat.input[peak.vec,]$d.2 <- dat.input[peak.vec,]$d.2*1.1