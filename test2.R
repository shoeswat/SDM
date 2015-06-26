require(plyr)
	require(ggplot2)
	require(raster)
	require(maptools)
	require(grid)
	
	xyfiles = list.files("/volumes/data/sdm/verify/presences", full.names=TRUE)	
	################INITIALIZE RESULT VECTORS################
	#general info
	names <- vector()
	nPres <- vector()
	nAbs <- vector()
	PA <- vector()

	
	#model eval vectors
	fittedTrees <- vector()
	cvThreshold = vector()
	cvThresholdSE = vector()
	discriminationMean = vector()
	discriminationMeanSE = vector()
	correlationMean = vector()
	correlationSE = vector()
	devianceMean = vector()
	devianceSE = vector()
	
	#elevation shift vectors
	modernMeans <- vector()
    modernMins <- vector()
    modernMaxes <- vector()
    modernSDs <- vector()
    holoceneMeans <- vector()
    holoceneMins <- vector()
    holoceneMaxes <- vector()
    holoceneSDs <- vector()
    lgmMeans <- vector()
    lgmMins <- vector()
    lgmMaxes <- vector()
    lgmSDs <- vector()

    
    #range change vectors
    ##for reference number of points with ends included
    modernPointsVectorWithOutliers <- vector()
	holocenePointsVectorWithOutliers <- vector()
	lgmPointsVectorWithOutliers <- vector()
    
	modernPointsVector <- vector()
	holocenePointsVector <- vector()
	lgmPointsVector <- vector()
	modernLandAreaVector <- vector()
	holoceneLandAreaVector <-vector()
	lgmLandAreaVector <-vector()
	holocene2presentLADelta <- vector()
	lgm2presentLADelta <- vector()
	lgm2holoceneLADelta <- vector()
	
	#latitude changes
	modern.lat.means = vector()
    modern.lat.mins = vector()
    modern.lat.maxes = vector()
    modern.lat.sd = vector()
    holocene.lat.means = vector()
    holocene.lat.mins = vector()
    holocene.lat.maxes = vector()
    holocene.lat.sd = vector()
    lgm.lat.means = vector()
    lgm.lat.mins = vector()
    lgm.lat.maxes = vector()
    lgm.lat.sd = vector()

	
	
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
		data.ready <- prep.species(coords, current_vars, nb.absences=10000)