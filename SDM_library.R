#################################################################################################################################
## NAME:	SDM_library.R
## PROJECT: Species Distribution Modelling
## DESC:	These are some functions with load and prep climate data and speicies occurance data.
## AUTHOR:	Scott Farley
##
## INPUTS:	See functions below
## RETURNS:	See functions below
## REQ:		Should be in same directory with server.R
##


	#############################################################################################################################
	## FUNCTION:	load.species()
	## DESC:		This file contains functions with main.R uses
	## INPUTS:		num (loads predictor)
	## RETURNS:		asdf
	load.predictors <-function(path){
	    vars = stack(list.files(path, pattern=".asc", full.names=TRUE)) ## stack combines each raster dataset into a single data structure
	    cat(paste("Loaded variables:",path,"\n"))
		return(vars)
	}

	#############################################################################################################################
	## FUNCTION:	load.species()
	## DESC:		This file contains functions with main.R uses
	## INPUTS:		asdf
	## RETURNS:		asdf
	prep.species <-function(species_xy, current_stack, nb.absences=4000, min.buffer.dist=10000, max.buffer.dist=1000000){
		library(dismo)
		source('circles_mod.R')
		presence <- data.frame(species_xy)
		presence$pres <-rep(1, nrow(presence))
		names(presence) <- c("x", "y", "pres")
		b1<-circles(species_xy, d=min.buffer.dist, lonlat=TRUE)
		b2<-circles(species_xy, d=max.buffer.dist, lonlat=TRUE)
		b1<-gUnionCascaded(b1@polygons)
		b2<-gUnionCascaded(b2@polygons)
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