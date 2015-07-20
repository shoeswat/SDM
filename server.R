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
current_vars <- load.predictors("./California/modern")
holocene_vars <-load.predictors("./California/midH")
lgm_vars<-load.predictors("./California/lgm")


shinyServer(function(input, output){

	# This function "reacts" to project button
	coords <- eventReactive(input$project, {

		# Query Ecoengine
		#responseData <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=1000&georeferenced=True&format=csv")))
		response <- getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=1000&georeferenced=True&format=csv"))

		# Return Different Data Sets for Different Types of Computations
		if(!is.null(input$customPresAbs) & nchar(response)!=0 ) {
			appendData <- read.csv(text = input$customPresAbs$datapath)
			responseData <- read.csv(text = response)
			names(responseData) <- c('lon','lat','X')
			names(appendData) <- c('lon','lat')
			return(rbind(responseData[,1:2],appendData))
		} else if (is.null(input$customPresAbs) & nchar(response)!=0) {
			return(read.csv(text = response)[,1:2])
		} else if (!is.null(input$customPresAbs) & nchar(response)==0) {
			uploadData <- read.csv(input$customPresAbs$datapath)
			#uploadData$pres <-rep(1, nrow(uploadData))
			return(uploadData)
		} else {
			stop("Please Upload Data or Enter a Valid Search")
		}

	})
		
	# Build a model from the response of the API (which should be presense points of species)
	model <- reactive({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Building Model", value = NULL)

		data.ready <- prep.species(coords(), current_vars, nb.absences=10000)
		#gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75)
		gbm.step(data.ready, 1:19, 'pres', tree.complexity=as.numeric(input$trDepth), learning.rate=as.numeric(input$lRate), max.trees=as.numeric(input$maxTrees), bag.fraction=as.numeric(input$bagFrac))
	})

	# Display model curve
	output$modelDiag <- renderPlot({
		x <- model()$trees.fitted
		y <- model()$cv.values
		plot(x,y,type = 'l',xlab = 'number of trees', ylab = 'cv deviance',main = "Crossvalidated Boosting Diagnostic")
		##lines(unlist(model[36]),unlist(model[38])+unlist(model[39]))
		##lines(unlist(model[36]),unlist(model[38])-unlist(model[39]))
	})

	# Display model statistics
	output$modelCVStats <- renderPrint({
		model()$cv.statistics
	})

	# Plot Projections
	output$modern <- renderPlot({
		# Display progress
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Projecting", value = NULL)

    	# Reclassify
    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
    	# Use model to figure out distribution
		modern = predict(current_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		# Reclassify probability of occurance to presense and absence
		modernBinary <- reclassify(modern, binaryReclass)
		# Plot projections
		plot(modernBinary, legend=FALSE, main="Modern Projection", xlab = "Longtitude", ylab = "Latitude")
		points(coords())
		if (!is.null(input$customPresAbs)){
			points(read.csv(input$coPlot$datapath), col = 'green', pch = 4)
		}
	})

	output$midH <- renderPlot({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "mid-Holocene", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		holocene = predict(holocene_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		holoceneBinary <- reclassify(holocene, binaryReclass)
		plot(holoceneBinary, legend=FALSE, main="mid-Holocene Projection", xlab = "Longtitude", ylab = "Latitude")
	})

	output$lgm <- renderPlot({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "Last Glacial Max", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		lgm = predict(lgm_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		lgmBinary <- reclassify(lgm, binaryReclass)
		plot(lgmBinary, legend=FALSE, main="LGM Projection", xlab = "Longtitude", ylab = "Latitude")
	})

})