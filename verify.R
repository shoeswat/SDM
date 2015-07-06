	runIter = 'test'

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
        i = 7
		f = xyfiles[i]
		name = gsub("/volumes/data/sdm/verify/presences/", "", f)
		name = gsub(".csv", "", name)
		coords = read.csv(f)
		coords = coords[c("longitude", "latitude")]
		coords = na.omit(coords)
		data.ready <- prep.species(coords, current_vars, nb.absences=10000)
		pa = as.vector(data.ready$pres)
		pres = pa[pa==1]
		abs = pa[pa==0]
		numPres = length(pres)
		numAbs = length(abs)
		paRatio = numPres/numAbs
		message(paste("Species:", name, "Data Generated...Now working on model creation."))
		nPres[i] <- numPres
		nAbs[i] <- numAbs
		PA[i] <- paRatio
		
		#build the model
		model <- gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000) 
		message("Model Created, Evaluating CV Statistics")
		
		#Evaluate CV results
	fittedTrees[i] = model$gbm.call$best.trees
    cvThreshold[i] = model$cv.statistics$cv.threshold
    cvThresholdSE[i] = model$cv.statistics$cv.threshold.se
    discriminationMean[i] = model$cv.statistics$discrimination.mean
    discriminationMeanSE[i] = model$cv.statistics$discrimination.se
    correlationMean[i] = model$cv.statistics$correlation.mean
    correlationSE[i] = model$cv.statistics$correlation.se
    devianceMean[i] = model$cv.statistics$deviance.mean
    devianceSE[i] = model$cv.statistics$deviance.se
    
    
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
    
    
    #reclassify to binary rasters
	modernBinary <- reclassify(modern, binaryReclass)
    holoceneBinary <- reclassify(holocene, binaryReclass)
    lgmBinary <- reclassify(lgm, binaryReclass)
    
    #how many points are within threshold
    modernPoints <- length(modernBinary[modernBinary== 1])
    holocenePoints <- length(holoceneBinary[holoceneBinary == 1])
    lgmPoints <- length(lgmBinary[lgmBinary == 1])	
	
	modernPointsVectorWithOutliers[i] <-modernPoints
    holocenePointsVectorWithOutliers[i] <- holocenePoints
    lgmPointsVectorWithOutliers[i] <- lgmPoints
    
    
    #elevation shift analyses
    modernPoints <- rasterToPoints(modernBinary)
    modernPoints<-data.frame(modernPoints)
    presencesModern <- modernPoints[modernPoints$layer == 1,]
    presLocsModern <- presencesModern[c("x", "y")]
    presElevationsModern <- extract(elevationRaster, presLocsModern)
    numModernPoints <- length(presElevationsModern)
    period = rep("modern", numModernPoints)
    modernElevDF <- data.frame(period, presElevationsModern, presLocsModern)
    
    holocenePoints <- rasterToPoints(holoceneBinary)
    holocenePoints<-data.frame(holocenePoints)
    presencesholocene <- holocenePoints[holocenePoints$layer == 1,]
    presLocsholocene <- presencesholocene[c("x", "y")]
    presElevationsholocene <- extract(elevationRaster, presLocsholocene)
    numholocenePoints <- length(presElevationsholocene)
    period = rep("holocene", numholocenePoints)
    holoceneElevDF <- data.frame(period, presElevationsholocene, presLocsholocene)
    
    lgmPoints <- rasterToPoints(lgmBinary)
    lgmPoints<-data.frame(lgmPoints)
    presenceslgm<- lgmPoints[lgmPoints$layer == 1,]
    presLocslgm <- presenceslgm[c("x", "y")]
    presElevationslgm <- extract(elevationRaster, presLocslgm)
    numlgmPoints <- length(presElevationslgm)
    period = rep("lgm", numlgmPoints)
    lgmElevDF <- data.frame(period, presElevationslgm, presLocslgm)
    
    message("Saving Elevation Points to .csv File")
    #combine all elevations into a file
    	#first rename so they can be combined
    names(modernElevDF) <- c("Period", "Elevation", "X", "Y")
    names(holoceneElevDF) <- c("Period", "Elevation", "X", "Y")
    names(lgmElevDF) <- c("Period", "Elevation", "X", "Y")
    #bind and save
    allTheElevations <- rbind(modernElevDF, holoceneElevDF, lgmElevDF)
    write.csv(allTheElevations, paste("/volumes/data/sdm/verify/reports/", runIter, "/Elevations.csv", sep=""))
    
    message("Eliminating Outliers")
    #eliminate outliers
	    modern.10 <- quantile(na.omit(presElevationsModern), .1)
        modern.90 <- quantile(na.omit(presElevationsModern), .9)
        holocene.10 <- quantile(na.omit(presElevationsholocene), .1)
        holocene.90 <- quantile(na.omit(presElevationsholocene), .9)
        lgm.10 <- quantile(na.omit(presElevationslgm), .1)
        lgm.90 <- quantile(na.omit(presElevationslgm), .9)
        
        modernNew <- modernElevDF[-which(modernElevDF$Elevation > modern.90),]
        modernNew <- modernNew[-which(modernNew$Elevation < modern.10),]
        holoceneNew <- holoceneElevDF[-which(holoceneElevDF$Elevation > holocene.90),]
        holoceneNew <- holoceneNew[-which(holoceneNew$Elevation < holocene.10),]
        lgmNew <- lgmElevDF[-which(lgmElevDF$Elevation > lgm.90),]
        lgmNew <- lgmNew[-which(lgmNew$Elevation < lgm.10),] 
       
       #bind and save
       new <- rbind(modernNew, holoceneNew, lgmNew)
        write.csv(new, paste("/volumes/data/sdm/verify/Reports/", runIter, "/", name, "_newElevations.csv", sep=""))
        
        message("Creating outlier-eliminated rasters")
        #get locations of elevations within the 10-90 range
        modernPoints <- modernNew[c("X", "Y")]
        holocenePoints <- holoceneNew[c("X", "Y")]
        lgmPoints <- lgmNew[c("X", "Y")]
        
        #convert to spatial points dataframe
        mSP <- data.frame(modernPoints, elev=modernNew$Elevation)
        coordinates(mSP) <- ~X+Y
        hSP <- data.frame(holocenePoints, elev=holoceneNew$Elevation)
        coordinates(hSP) <- ~X+Y
        lSP <- data.frame(lgmPoints, elev=lgmNew$Elevation)
        coordinates(lSP) <- ~X+Y
        
        ##set up the output rasters 
        e <- extent(current_vars[[1]])
        a <- res(current_vars[[1]])
        r <- nrow(current_vars[[1]])
        c <- nrow(current_vars[[1]])
        p <- projection(current_vars[[1]])
        m <- raster(ext=e, nrow=r, ncol=c, crs=p)
        h<- raster(ext=e, nrow=r, ncol=c, crs=p)
        l<- raster(ext=e, nrow=r, ncol=c, crs=p)
        
        #fill the rasters with the point vals
        m <- rasterize(mSP, m, field='elev', fun=mean)
        h <- rasterize(hSP, h, field='elev', fun=mean)
        l <- rasterize(lSP, l, field='elev', fun=mean)
        
        #Save the results
        message("Saving outlier-eliminated result rasters")
        writeRaster(m, paste("/volumes/data/sdm/verify/OutliersDeleted/Modern/", runIter, "/ModernSansOutliers.grd", sep=""), overwrite=TRUE)
        writeRaster(h, paste("/volumes/data/sdm/verify/OutliersDeleted/Holocene/", runIter, "/HoloceneSansOutliers.grd", sep=""), overwrite=TRUE)
        writeRaster(l, paste("/volumes/data/sdm/verify/OutliersDeleted/LGM/", runIter, "/LGMSansOutliers.grd", sep=""), overwrite=TRUE)
        
        	
	modernPointsVectorWithOutliers[i] <- length(modernNew)
    holocenePointsVectorWithOutliers[i] <- length(holoceneNew)
    lgmPointsVectorWithOutliers[i] <- length(lgmNew)
    
    #land area analysis
    modernLandArea <- length(modernNew) * .64 #convert to square kms (.8km * .8km)
    holoceneLandArea <- length(holoceneNew) * .64
    lgmLandArea <- length(lgmNew) *.64
    
    modernLandAreaVector[i] <- modernLandArea
    holoceneLandAreaVector[i] <- holoceneLandArea
    lgmLandAreaVector[i] <- lgmLandArea
    
    #calculate differences between periods
    holocene2presentDelta <- modernLandArea - holoceneLandArea
    holocene2presentLADelta[i] <- holocene2presentDelta
    
    lgm2presentDelta <- modernLandArea - lgmLandArea
    lgm2presentLADelta[i] <- lgm2presentDelta
    
    lgm2holoceneDelta <- holoceneLandArea - lgmLandArea
    lgm2holoceneLADelta[i] <- lgm2holoceneDelta
    
    #elevation shift analyses
    presElevationsModern <- na.omit(modernNew$Elevation)
    modernMin = min(presElevationsModern)
    modernMax = max(presElevationsModern)
    modernMean = mean(presElevationsModern)
    modernSD = sd(presElevationsModern)
    modernMeans[i] = modernMean
    modernMins[i] = modernMin
    modernMaxes[i] = modernMax
    modernSDs[i] = modernSD
    
	presElevationsholocene <- na.omit(holoceneNew$Elevation)
    holoceneMin = min(presElevationsholocene)
    holoceneMax = max(presElevationsholocene)
    holoceneMean = mean(presElevationsholocene)
    holoceneSD = sd(presElevationsholocene)
    holoceneMeans[i] = holoceneMean
    holoceneMins[i] = holoceneMin
    holoceneMaxes[i] = holoceneMax
    holoceneSDs[i] = holoceneSD
    
	presElevationslgm <- na.omit(lgmNew$Elevation)
    lgmMin = min(presElevationslgm)
    lgmMax = max(presElevationslgm)
    lgmMean = mean(presElevationslgm)
    lgmSD = sd(presElevationslgm)
    lgmMeans[i] = lgmMean
    lgmMins[i] = lgmMin
    lgmMaxes[i] = lgmMax
    lgmSDs[i] = lgmSD
    
    modernNew = na.omit(modernNew)
    holoceneNew = na.omit(holoceneNew)
    lgmNew = na.omit(lgmNew)
        #Create some plots
        message("Plotting...")
        ##First Elevation vs. Latitude Plots
        	#calc 75th percentile
        modern.75 <- ddply(modernNew, "Y", summarise, elev.max=quantile(Elevation, .75))
        holocene.75 <- ddply(holoceneNew, "Y", summarise, elev.max=quantile(Elevation, .75))
        lgm.75 <- ddply(lgmNew, "Y", summarise, elev.max=quantile(Elevation, .75))
        
     	saveName = paste("/volumes/data/sdm/verify/plots/", runIter, "/latPlots/", name, ".pdf", sep="")
         pdf(saveName)
         plot(elev.max=quantile(Elevation, .75), type="l", col="black", main=paste("Latitudinal Transect Analysis\n",name, "\nOutliers Deleted", sep=""), sub="75th Percentile Elevation Values", xlab="Latitude", ylab="Elevation")
        lines(modern.75, col="blue")
         lines(holocene.75, col='red')
        lines(lgm.75, col='green')
        legend("topleft", legend=c("Elevation", "Modern", "mid-Holocene", "LGM"), fill=c("black", "blue", "Red", "green"))
        dev.off()
        
        
        #Next do histograms, density plots and boxplots
         modern.mean <- mean(modernNew$Elevation)
         holocene.mean <- mean(holoceneNew$Elevation)
         lgm.mean <- mean(lgmNew$Elevation)
          fileName1 = paste("/volumes/data/sdm/verify/plots/", runIter, "/boxplots/", name, ".pdf", sep="")
         fileName2 = paste("/volumes/data/sdm/verify/plots/", runIter, "/densityPlots/", name, ".pdf", sep="")
         fileName3 = paste("/volumes/data/sdm/verify/plots/", runIter, "/histograms/", name, ".pdf", sep="")
        p <- ggplot(new, aes(x=Period, y=Elevation, fill=Period)) + geom_boxplot() + ggtitle(paste(name, "\nElevation by Period")) + xlab("Time Period") + ylab("Elevation") 
          ggsave(fileName1)
          ggplot(new, aes(x= Elevation, fill=Period)) + geom_density(alpha=0.3, binwidth=100) + ggtitle(paste(name, "\nDensity of Elevation Points")) + xlab("Elevation") + ylab("Density")
         ggsave(fileName2)
          ggplot(new, aes(x= Elevation, fill=Period)) + geom_histogram(alpha=0.3, binwidth=100) + ggtitle(paste(name, "\nDistribution of Elevation Points")) + xlab("Elevation") + ylab("Number of Gridpoints")
          ggsave(fileName3)
          
          
          message("Doing Latitude Shift Analysis")
          ###Do latitude shift analysis
        modernE = as.vector(modernNew$Y)
        m.mean = mean(modernE)
        m.max = max(modernE)
        m.min = min(modernE)
        m.sd = sd(modernE)
        modern.lat.means[i] = m.mean
        modern.lat.maxes[i] = m.max
        modern.lat.mins[i] = m.min
        modern.lat.sd[i] = m.sd
        
        holoE = as.vector(holoceneNew$Y)
        h.mean = mean(holoE)
        h.min = min(holoE)
        h.max = max(holoE)
        h.sd = sd(holoE)
        holocene.lat.means[i] = h.mean
        holocene.lat.mins[i] = h.min
        holocene.lat.maxes[i] = h.max
        holocene.lat.sd[i] = h.sd
        
        lgmE = as.vector(lgmNew$Y)
        l.mean = mean(lgmE)
        l.min = min(lgmE)
        l.max = max(lgmE)
        l.sd = sd(lgmE)
        lgm.lat.means[i] = l.mean
        lgm.lat.maxes[i] = l.max
        lgm.lat.mins[i] = l.min
        lgm.lat.sd[i] = l.sd

        message("Species Completed...")   
        i = i +1
		
	overview = data.frame(names, nPres, nAbs, PA, fittedTrees, cvThreshold, cvThresholdSE, discriminationMean, discriminationMeanSE, correlationMean, correlationSE, devianceMean, devianceSE)
	write.csv(overview, paste("/volumes/data/sdm/verify/reports/", runIter, "/overview.csv"))
	elevation = data.frame(names, modernMeans, modernMins, modernMaxes, modernSDs, holoceneMeans, holoceneMins, holoceneMaxes, holoceneSDs, lgmMeans, lgmMins, lgmMaxes, lgmSDs)
	write.csv(elevation, paste("/volumes/data/sdm/verify/reports/", runIter, "/elevationSummary.csv"))
	lat = data.frame(names, modern.lat.means, modern.lat.mins, modern.lat.maxes, modern.lat.sd, holocene.lat.means, holocene.lat.mins, holocene.lat.maxes, holocene.lat.sd, lgm.lat.means, lgm.lat.mins, lgm.lat.maxes, lgm.lat.sd)
	write.csv(lat, paste("/volumes/data/sdm/verify/reports/", runIter, "/latitudeSummary.csv"))
	range = data.frame(names, modernPointsVector, holocenePointsVector, lgmPointsVector, modernLandAreaVector, holoceneLandAreaVector, lgmLandAreaVector, holocene2presentLADelta, lgm2presentLADelta, lgm2holoceneLADelta)
	write.csv(range, paste("/volumes/data/sdm/verify/reports/", runIter, "/rangeSummary.csv"))
	allTheData <- data.frame(names, nPres, nAbs, PA, fittedTrees, cvThreshold, cvThresholdSE, discriminationMean, discriminationMeanSE, correlationMean, correlationSE, devianceMean, devianceSE,modernMeans, modernMins, modernMaxes, modernSDs, holoceneMeans, holoceneMins, holoceneMaxes, holoceneSDs, lgmMeans, lgmMins, lgmMaxes, lgmSDs, modern.lat.means, modern.lat.mins, modern.lat.maxes, modern.lat.sd, holocene.lat.means, holocene.lat.mins, holocene.lat.maxes, holocene.lat.sd, lgm.lat.means, lgm.lat.mins, lgm.lat.maxes, lgm.lat.sd, modernPointsVector, holocenePointsVector, lgmPointsVector, modernLandAreaVector, holoceneLandAreaVector, lgmLandAreaVector, holocene2presentLADelta, lgm2presentLADelta, lgm2holoceneLADelta)
	write.csv(allTheData, paste("/volumes/data/sdm/verify/reports/", runIter, "/RunResultsComplete.csv"))
	return(allTheData)