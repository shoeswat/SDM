##example script to run through a single species and produce diagrams like those in the paper
##June 25, 2015
##Require necessary libraries
## most of these have dependencies that must also be installed
##some of these are not required for the basic example, but are important for the post-processing and graphing of results
require(rgdal) ##underlying GIS tools (dependency for other spatial packages)
require(gbm) ##dependency for boosted regression functions in dismo
require(plyr)  ##helps with manipualting data frames (not essential for modeling)
require(ggplot2)  ##makes good looking graphics (not essential for modeling)
require(raster) ##raster data structure manipulation (essential)
require(maptools) ## manipulation of shapefiles (helpful in preprocessing)
require(dismo) ##holds function for models (essential)
require(ecoengine) ##API for downloading presence points from ecoengine.berkeley.edu --> exahustive source of presence points for CA only
require(rgeos) ## necessary for geometry manipulation for the psuedo-absence point generation

##for reclassifying raster to 40% prob of pres
binaryReclass <- matrix(c(0, 0.4, 0, 0.4, 1, 1), byrow=TRUE, ncol=3)

## load the species presence points
load.species <- function(scientific_name){
  records <-ee_observations(scientific_name__exact=scientific_name, georeferenced=TRUE, page="all")  ## get all the records from the ecoengine that are geotagged and match the genius and species
  records.data <-records$data  ## there is a lot of metadata that we dont need
  records.xy<-records.data[, c("longitude", "latitude")] ## now only take the lat/lng --> we only need to know where it was found right now
  records.xy<-data.frame(records.xy)
  return (records.xy)
}
  
##load the predictor rasters (grids of environmental gradients) from disc --> !!!!this must be modified for your computer!!!
load.predictors <-function(num){
  library(raster)
  if (num==1){
    present.files <-list.files("/volumes/lacie/sdm/california_bvc/present_asc", pattern=".asc", full.names=TRUE)
    present.files <-present.files[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37)]
    vars = stack(present.files) ## stack combines each raster dataset into a single data structure
    cat("Loaded present variables\n")
  }
  else if(num==2){
    holocene.files <-list.files("/volumes/lacie/sdm/california_bvc/holocene_asc", pattern=".asc", full.names=TRUE)
    holocene.files <-holocene.files[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37)]
    vars = stack(holocene.files)
    cat("Loaded holocene variables\n")
  }
  else if(num==3){
    lgm.files <-list.files("/volumes/lacie/sdm/california_bvc/lgm_asc", pattern=".asc", full.names=TRUE)
    vars = stack(lgm.files[1:19])
    cat("Loaded lgm files")
    
  }
  return(vars)
}

##method for creating presence/absence matrix
##uses custom method for identifying absence points for the species
prep.species <-function(species_xy, current_stack, nb.absences=4000, min.buffer.dist=10000, max.buffer.dist=1000000){
  ##species_xy == dataframe created by load.species()
  ##current_stack --> current variables stack equivalent to load.predictors(1)
  ##nb.absences --> number of absence points to generate (this will be different than the total at the end because some are in the ocean or otherwise elminated)
  ##min.buffer.dist --> minimum number of meters between each of the presence points and the absence points zone
  ##max.buffer.dist --> maxmimum number of meters between each of the presence points 
  ##max.buffer.dist - min.buffer distance = zone to draw absence points from
  ##some of this zone will be outside of CA study area, in the ocean, etc, so these are elimated.  All points remaining in the zone become absence points for the experiment.
  presence <- data.frame(species_xy)
  presence$pres <-rep(1, nrow(presence)) ##everywhere where the ecoengine says the species occur, assign a '1'
  names(presence) <- c("x", "y", "pres")   ##rename the headers to maintain consistancy
  b1 <-circles(species_xy, d=min.buffer.dist, lonlat=TRUE) ## draw the buffers
  b2<-circles(species_xy, d=max.buffer.dist, lonlat=TRUE)
  b1<-gUnaryUnion(b1@polygons) ## make spatial entity from which to draw absence points
  b2<-gUnaryUnion(b2@polygons)
  samplezone<-gDifference(b2, b1) 
  abs.samples<-spsample(samplezone, nb.absences, type='random', iter=25) ##do the sampling
  cells<-cellFromXY(current_stack, abs.samples) ##elimate the drawn points that do not have any environmental information associated with them
  cells<-unique(cells) ## and there can only be one absence point per grid-cell (800 m^2)
  abs.xy <-xyFromCell(current_stack, cells)
  abs<-data.frame(abs.xy) 
  abs$pres <- rep(0, nrow(abs))
  names(abs)<-c("x", "y", "pres")
  data<-rbind(presence, abs)##put it all together so that absence points are (x, y, 0) and presence points are (x, y, 1) all in a single data frame
  data.xy <- data.frame(data[, c("x", "y")])
  data.vals<-extract(current_stack, data.xy) ## extract the environmental gradient conditions for each of the 19 variables in the stack for each presence and each absence point
  data.vals<-data.frame(data.vals)
  data.vals$pres<-data$pres
  data.ready <-na.omit(data.vals) ## get rid of any straggling points that have a NA value so they dont mess anything up
  return (data.ready)	## this data can be fed into the modeling function
}

##load the predictors into memory
current_vars <- load.predictors(1)
holocene_vars <-load.predictors(2)
lgm_vars<-load.predictors(3)

##prep the data 
coords <- load.species('quercus agrifolia')
data.ready <- prep.species(coords, current_vars, nb.absences=10000)

##build the model --> this step takes a while
model <- gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000) 
##gbm.step is from the dismo package --> there is a good walkthrough of these functions if you google dismo
##data.ready --> prep.species output
##1:19 --> use columns 1:19 (all of them) for model building.  If you want to use a subset use like this -> c(1, 5,...8) //list
##tree.complexity --> number of interactions allowed per tree
##learning.rate --> additional information provided by each new tree added to the model
##Refer to the documentation for this function for proper use of these params

##now the model is build we can use it
##predict the modern range of the species from the modern presence points --> this can be used to check distributions against USGS reports, etc
modern = predict(current_vars, model, n.trees=model$gbm.call$best.trees, type='response')
##predict is part of dismo package
##current_vars --> current stack of environmental variables --> should be the same as the one used to build the model
##n.trees --> the number of trees to use --> you must set this param but should be "model$gbm.call$best.trees" to get the right result
##type --> you can predict with multiple types of models with this function.  Make sure this is set to response
##optional --> progress='text' --> gives updates which can be useful because it takes a long time to predict some models

#!!!Change these destinations for your filesystems!!!!
writeRaster(modern, paste("/volumes/lacie/sdm/verify/modernGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE) ##Save the result to disc
##now hindcast and 'predict' what the range would have been given modern species affinities but mid-holocene environmental conditions
holocene = predict(holocene_vars, model, n.trees=model$gbm.call$best.trees, type='response')
writeRaster(holocene, paste("/volumes/lacie/sdm/verify/holoceneGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE) 
## and again for the last glacial maximum
lgm = predict(lgm_vars, model, n.trees=model$gbm.call$best.trees, type='response')
writeRaster(lgm, paste("/volumes/lacie/sdm/verify/lgmGrids/", runIter, "/", name, ".grd", sep=""), overwrite=TRUE)

#reclassify to binary rasters
##using the reclassify matrix, take the distribution from a continuous (0, 1) probability of presence to a binary P/A dataset
modernBinary <- reclassify(modern, binaryReclass)
holoceneBinary <- reclassify(holocene, binaryReclass)
lgmBinary <- reclassify(lgm, binaryReclass)



###BASIC EXAMPLE END####
#########################
##more info....

##to see the results
plot(modernBinary)
plot(holoceneBinary)
plot(lgmBinary)

##Statistics of the model
modelStats <- model$cv.statistics

##Additional model info
summary(model)
gbm.interactions(model)



