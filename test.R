requireNamespace('rgeos')
require(rgeos)

load.species <- function(scientific_name){
	library(ecoengine)
	records <-ee_observations(scientific_name__exact=scientific_name, georeferenced=TRUE, page="all")
	records.data <-records$data
	records.xy<-records.data[, c("longitude", "latitude")]
	records.xy<-data.frame(records.xy)
	return (records.xy)
}
load.predictors <-function(num){
    library(raster)
	if (num==1){
		present.files <-list.files("/volumes/data/sdm/california_bvc/present_asc", pattern=".asc", full.names=TRUE)
		present.files <-present.files[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37)]
		vars = stack(present.files)
		cat("Loaded present variables\n")
	}
	else if(num==2){
		holocene.files <-list.files("/volumes/data/sdm/california_bvc/holocene_asc", pattern=".asc", full.names=TRUE)
		holocene.files <-holocene.files[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37)]
		vars = stack(holocene.files)
		cat("Loaded holocene variables\n")
	}
	else if(num==3){
		lgm.files <-list.files("/volumes/data/sdm/california_bvc/lgm_asc", pattern=".asc", full.names=TRUE)
		vars = stack(lgm.files[1:19])
		cat("Loaded lgm files")
		
	}
	else{
		cat("Not an option :(")
	}
	return(vars)
}


current_vars <- load.predictors(1)


	require(plyr)
	require(ggplot2)
	require(raster)
	require(maptools)
	require(grid)
	
	xyfiles = list.files("/volumes/data/sdm/verify/presences", full.names=TRUE)	

	
	##for reclassifying raster to 40% prob of pres
	binaryReclass <- matrix(c(0, 0.4, 0, 0.4, 1, 1), byrow=TRUE, ncol=3)
	
	#for elevation analysis 
	elevationRaster = raster("/Volumes/data/SDM/Types/CA_elev_clipped.grd")
	
	numSpecies = length(xyfiles)
	
	##start main loop 
	#for (i in 1:numSpecies){
        i = 8
		f = xyfiles[i]
		name = gsub("/volumes/data/sdm/verify/presences/", "", f)
		name = gsub(".csv", "", name)
		coords = read.csv(f)
		coords = coords[c("longitude", "latitude")]
		coords = na.omit(coords)

species_xy = coords
current_stack = current_vars
min.buffer.dist=10000
 
library(dismo)
#library(rgeos)
presence <- data.frame(species_xy)
presence$pres <-rep(1, nrow(presence))
names(presence) <- c("x", "y", "pres")

b1<-circles(species_xy, d=min.buffer.dist, lonlat=TRUE)