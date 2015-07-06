#################################################################################################################################
## NAME:	SDM_library.R
## PROJECT: Species Distribution Modelling
## DESC:	This file contains functions with main.R uses
## AUTHOR:	Scott Farley
##
## INPUTS:	(none)
## RETURNS:	(null)
## REQ:		Should be in same directory with main.R
##
## HISTORY:
## 07/02/2015 (Yugarshi Mondal) - orginal refactor 

	#############################################################################################################################
	## FUNCTION:	load.species()
	## DESC:		This file contains functions with main.R uses
	## INPUTS:		asdf
	## RETURNS:		asdf
	load.predictors <-function(num){
	  library(raster)
	  if (num==1){
	    present.files <-list.files("/volumes/data/sdm/california_bvc/present_asc", pattern=".asc", full.names=TRUE)
	    present.files <-present.files[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37)]
	    vars = stack(present.files) ## stack combines each raster dataset into a single data structure
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
	  return(vars)
	}

	#############################################################################################################################
	## FUNCTION:	load.species()
	## DESC:		This file contains functions with main.R uses
	## INPUTS:		asdf
	## RETURNS:		asdf
	prep.species <-function(species_xy, current_stack, nb.absences=4000, min.buffer.dist=10000, max.buffer.dist=1000000){
		#library(rgeos)
		library(dismo)
		presence <- data.frame(species_xy)
		presence$pres <-rep(1, nrow(presence))
		names(presence) <- c("x", "y", "pres")
		b1 <-circles(species_xy, d=min.buffer.dist, lonlat=TRUE)
		b2<-circles(species_xy, d=max.buffer.dist, lonlat=TRUE)
		b1<-gUnaryUnion(b1@polygons)
		b2<-gUnaryUnion(b2@polygons)
		samplezone<-gDifference(b2, b1)
		abs.samples<-spsample(samplezone, nb.absences, type='random', iter=25)
		cells<-cellFromXY(current_stack, abs.samples)
		cells<-unique(cells)
		abs.xy <-xyFromCell(current_stack, cells)
		abs<-data.frame(abs.xy)
		abs$pres <- rep(0, nrow(abs))
		names(abs)<-c("x", "y", "pres")
		data<-rbind(presence, abs)
		data.xy <- data.frame(data[, c("x", "y")])
		data.vals<-extract(current_stack, data.xy)
		data.vals<-data.frame(data.vals)
		data.vals$pres<-data$pres
		data.ready <-na.omit(data.vals)
		return (data.ready)	
	}

	#############################################################################################################################
	## FUNCTION:	load.species()
	## DESC:		This file contains functions with main.R uses
	## INPUTS:		asdf
	## RETURNS:		asdf
	doModelStack <-function(data, current.vars, holocene.vars, lgm.vars, predictors=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), presence='pres', lr=0.005, tc=10){
		library(dismo)
		model = gbm.step(data, gbm.x=predictors, gbm.y=presence, tree.complexity=tc, learning.rate=lr)
		summary<-summary(model)
		find.ints<-gbm.interactions(model)
		interactions<-find.ints$rank.list
		current_predictions<-predict(current.vars, model, n.trees=model$gbm.call$best.trees, type='response', progress='text')
		holocene_predictions<-predict(holocene.vars, model, n.trees=model$gbm.call$best.trees, type='response', progress='text')
		lgm_predictions<-predict(lgm.vars, model, n.trees=model$gbm.call$best.trees, type='response', progress='text')
	}