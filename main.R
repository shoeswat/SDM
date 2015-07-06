#################################################################################################################################
## NAME:	main.R
## PROJECT: Species Distribution Modelling
## DESC:	This is the main script that creates SDM and plots.
## AUTHOR:	Scott Farley
##
## INPUTS:	(null)
## RETURNS:	(null)
## REQ:		SDM_library.R should be in same directory as main.R
##
## HISTORY:
## 07/02/2015 (Yugarshi Mondal) - orginal refactor 

# Load Required Libraries
requireNamespace('rgeos')
require(rgeos)
require(plyr)
require(ggplot2)
require(raster)
require(maptools)
require(grid)
library(RCurl)

# Load SDM library
source('SDM_library.R')

runIter = 'test'
name = 'Tsuga'

current_vars <- load.predictors(1)
holocene_vars <-load.predictors(2)
lgm_vars<-load.predictors(3)

## Uses ecoengine API to retrieve CSV data
## read.csv( text = getURL("URL") )
coords <- read.csv(text = getURL("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22Tsuga%20heterophylla%22&fields=geojson&page_size=300&georeferenced=True&format=csv"))
coords <- coords[,1:2]
names(coords) <- c("longitude", "latitude")
data.ready <- prep.species(coords, current_vars, nb.absences=10000)
pa = as.vector(data.ready$pres)
pres = pa[pa==1]
abs = pa[pa==0]
numPres = length(pres)
numAbs = length(abs)
paRatio = numPres/numAbs

# Build Model
message(paste("Species:", name, "Data Generated...Now working on model creation."))
model <- gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75) 

# Use Model to Predict
#project onto climate grids
message("Statistics Evaluated, proceeding to analysis steps...First Projecting onto modern surface...")
modern = predict(current_vars, model, n.trees=model$gbm.call$best.trees, type='response')
writeRaster(modern, paste("/volumes/data/sdm/verify/modernGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE)
message("modern complete")
holocene = predict(holocene_vars, model, n.trees=model$gbm.call$best.trees, type='response')
writeRaster(holocene, paste("/volumes/data/sdm/verify/holoceneGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE)
message("Holocene complete")
lgm = predict(lgm_vars, model, n.trees=model$gbm.call$best.trees, type='response')
writeRaster(lgm, paste("/volumes/data/sdm/verify/lgmGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE)
message("Proceeding to eliminate outliers...")
    
##for reclassifying raster to 40% prob of pres
binaryReclass <- matrix(c(0, 0.4, 0, 0.4, 1, 1), byrow=TRUE, ncol=3)

#reclassify to binary rasters
modernBinary <- reclassify(modern, binaryReclass)
holoceneBinary <- reclassify(holocene, binaryReclass)
lgmBinary <- reclassify(lgm, binaryReclass)

##to see the results
plot(modernBinary)
points(coords)
plot(holoceneBinary)
plot(lgmBinary)
