#################################################################################################################################
## NAME:	server.R
## PROJECT: Species Distribution Modelling
## DESC:	This is the server side of R shiny app -- it recomputes model every time "actionButton" in ui.R is clicked.
## AUTHOR:	Yugarshi Mondal
##
## INPUTS:	(none)
## RETURNS:	(null)
## REQ:		Should be in same directory with ui.R and SDM_library.R
##

#Sequoia sempervirens
#Tsuga heterophylla

## Global Libraries
library(shiny)
require(rgeos)
library(dismo)
require(plyr)
require(ggplot2)
require(raster)
require(maptools)
require(grid)
library(RCurl)
source('SDM_library.R')

## Load Data Stack
current_vars <- load.predictors("./deployedData/bioClim/modern")
holocene_vars <-load.predictors("./deployedData/bioClim/midH")
lgm_vars<-load.predictors("./deployedData/bioClim/lgm")
elevationRaster <- raster("./deployedData/CA_elev_clipped.grd")

## Other Static Variables
bioVars = c("BIO1 - Annual Mean Temperature","BIO2 - Mean Diurnal Range (Mean of monthly (max temp - min temp))","BIO3 - Isothermality (BIO2/BIO7)","BIO4 - Temperature Seasonality (standard deviation)","BIO5 - Max Temperature of Warmest Month","BIO6 - Min Temperature of Coldest Month","BIO7 - Temperature Annual Range (BIO5-BIO6)","BIO8 - Mean Temperature of Wettest Quarter","BIO9 - Mean Temperature of Driest Quarter","BIO10 - Mean Temperature of Warmest Quarter","BIO11 - Mean Temperature of Coldest Quarter","BIO12 - Annual Precipitation","BIO13 - Precipitation of Wettest Month","BIO14 - Precipitation of Driest Month","BIO15 - Precipitation Seasonality (Coefficient of Variation)","BIO16 - Precipitation of Wettest Quarter","BIO17 - Precipitation of Driest Quarter","BIO18 - Precipitation of Warmest Quarter","BIO19 - Precipitation of Coldest Quarter")

shinyServer(function(input, output){

	# This function "reacts" to project button
	coords <- eventReactive(input$project, {

		# Query Ecoengine
		#responseData <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode("Tsuga heterophylla"),"%22&fields=geojson&page_size=1000&georeferenced=True&format=csv")))
		response <- getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=1000&georeferenced=True&format=csv"))

		# Return Different Data Sets for Different Types of Computations
		if(!is.null(input$customPresAbs) & nchar(response)!=0 ) {
			appendData <- read.csv(input$customPresAbs$datapath)
			responseData <- read.csv(text = response)
			names(responseData) <- c('lon','lat','X')
			names(appendData) <- c('lon','lat')
			return(rbind(responseData[,1:2],appendData))
		} else if (is.null(input$customPresAbs) & nchar(response)!=0) {
			return(read.csv(text = response)[,1:2])
		} else if (!is.null(input$customPresAbs) & nchar(response)==0) {
			uploadData <- read.csv(input$customPresAbs$datapath)
			uploadData$pres <-rep(1, nrow(uploadData))
			return(uploadData)
		} else {
			stop("Please Upload Data or Enter a Valid Search")
		}

	})
		
	# Build a model from the response of the API (which should be presense points of species)
	model <- reactive({
		message(dev.cur())
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Building Model", value = NULL)

		data.ready <- prep.species(coords(), current_vars, nb.absences=10000)
		#gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75)
		gbm.step(data.ready, 1:19, 'pres', tree.complexity=as.numeric(input$trDepth), learning.rate=as.numeric(input$lRate), max.trees=as.numeric(input$maxTrees), bag.fraction=as.numeric(input$bagFrac))
		message(dev.cur())
	})

	# Display model curve
	output$modelDiag <- renderPlot({
		x <- model()$trees.fitted
		y <- model()$cv.values
		plot(x,y,type = 'l',xlab = 'number of trees', ylab = 'cv deviance',main = "Crossvalidated Boosting Diagnostic")
		##lines(unlist(model[36]),unlist(model[38])+unlist(model[39]))
		##lines(unlist(model[36]),unlist(model[38])-unlist(model[39]))
	})

	# Print model Diagnostics
	output$modelCVStats <- renderPrint({
		model()$cv.statistics
	})
	output$numTrees <- renderPrint({
		model()$n.trees
	})
	output$contributions <- renderPrint({
		temp = data.frame(cbind(model()$var.names,bioVars))
		names(temp) <- c("var","Variable")
		temp2 <- merge(model()$contributions, temp, by = "var", all = TRUE, sort = F)
		return(temp2[,2:3])
	})

	# Display Reference Climate Data
	output$precipRef <- renderImage({
		# render image must return a list
		return(list(src = "./deployedData/climateRefs/precip.png", contentType = 'image/png'))
	}, deleteFile = FALSE)
	output$tasRef <- renderImage({
		# render image must return a list
		return(list(src = "./deployedData/climateRefs/tas.png", contentType = 'image/png'))
	}, deleteFile = FALSE)
	output$tminRef <- renderImage({
		# render image must return a list
		return(list(src = "./deployedData/climateRefs/tmin.png", contentType = 'image/png'))
	}, deleteFile = FALSE)
	output$tmaxRef <- renderImage({
		# render image must return a list
		return(list(src = "./deployedData/climateRefs/tmax.png", contentType = 'image/png'))
	}, deleteFile = FALSE)


	#Create a reactive for binaries to be used in other functions
	modernBinary <- reactive({
		if (is.null(model()$data)) return()

		# Display progress
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Projecting", detail = "Modern", value = NULL)

		# Reclassify
    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
    	# Use model to figure out distribution
		modern = predict(current_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		# Reclassify probability of occurance to presense and absence
		modernBinary <- reclassify(modern, binaryReclass)
		return(modernBinary)
	})
	holoceneBinary <- reactive({
		if (is.null(modernBinary())) return()

		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "mid-Holocene", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		holocene = predict(holocene_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		holoceneBinary <- reclassify(holocene, binaryReclass)
		return(holoceneBinary)

	})
	lgmBinary <- reactive({
		if (is.null(modernBinary())) return()

		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "Last Glacial Max", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		lgm = predict(lgm_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		lgmBinary <- reclassify(lgm, binaryReclass)
		return(lgmBinary)

	})


	# Plot Projections
	output$modern <- renderPlot({
		if (is.null(modernBinary())) return()

		message(dev.cur())
		# Plot projections
		d2 <- crop(modernBinary(),c(as.numeric(input$lonWest),as.numeric(input$lonEast),as.numeric(input$latSouth),as.numeric(input$latNorth)))
		plot(d2, legend=FALSE, main="Modern Projection", xlab = "Longtitude", ylab = "Latitude")
		points(coords())
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		message(dev.cur())
	})
	output$midH <- renderPlot({
		if (is.null(modernBinary())) return()

		message(dev.cur())
		#plot(holoceneBinary, legend=FALSE, main="mid-Holocene Projection", xlab = "Longtitude", ylab = "Latitude")
		d1 <- (modernBinary()-holoceneBinary())+(2*(modernBinary()*holoceneBinary()))
		d2 <- crop(d1,c(as.numeric(input$lonWest),as.numeric(input$lonEast),as.numeric(input$latSouth),as.numeric(input$latNorth)))
		plot(d2, legend = FALSE, col = c('#D55E00','#E6E6E6','#56B4E9','#009E73'), main="mid-Hol Presence Anomaly", xlab = "Longtitude", ylab = "Latitude", cex = .5)
		legend("topright", legend = c("midH Only", "Neither", "Modern Only", "Both"), fill = c('#D55E00','#E6E6E6','#56B4E9','#009E73'))
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		message(dev.cur())
	})
	output$lgm <- renderPlot({
		if (is.null(modernBinary())) return()

		message(dev.cur())
		#plot(lgmBinary, legend=FALSE, main="LGM Projection", xlab = "Longtitude", ylab = "Latitude")
		d1 <- (modernBinary()-lgmBinary())+(2*(modernBinary()*lgmBinary()))
		d2 <- crop(d1,c(as.numeric(input$lonWest),as.numeric(input$lonEast),as.numeric(input$latSouth),as.numeric(input$latNorth)))
		plot(d2, legend = FALSE, col = c('#D55E00','#E6E6E6','#56B4E9','#009E73'), main="LGM Presence Anomaly", xlab = "Longtitude", ylab = "Latitude", cex = .75)
		legend("topright", legend = c("LGM Only", "Neither", "Modern Only", "Both"), fill = c('#D55E00','#E6E6E6','#56B4E9','#009E73'))
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		message(dev.cur())
	})

	# Elevation Analysis
	elevData <- reactive({
		if (is.null(modernBinary())) return()

		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Analyzing Elevations...", value = NULL)

		n1 <- analyzeElevation(modernBinary(),elevationRaster,"Modern")
		n2 <- analyzeElevation(holoceneBinary(),elevationRaster,"midH")
		n3 <- analyzeElevation(lgmBinary(),elevationRaster,"LGM")
		elData <- rbind(n1, n2, n3)
		return(elData)
	})

	# Elevation Plots
	output$elev1 <- renderPlot({
		if (is.null(elevData())) return()

		ggplot(elevData(), aes(x=Period, y=Elevation, fill=Period)) + geom_boxplot() + ggtitle("Elevation by Period") + xlab("Time Period") + ylab("Elevation") 
	})
	output$elev2 <- renderPlot({
		if (is.null(elevData())) return()

		ggplot(elevData(), aes(x= Elevation, fill=Period)) + geom_density(alpha=0.3, binwidth=100) + ggtitle("Density of Elevation Points") + xlab("Elevation") + ylab("Density")	})
	output$elev3 <- renderPlot({
		if (is.null(elevData())) return()

		ggplot(elevData(), aes(x= Elevation, fill=Period)) + geom_histogram(alpha=0.3, binwidth=100) + ggtitle("Distribution of Elevation Points") + xlab("Elevation") + ylab("Number of Gridpoints")
	})
})
