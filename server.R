#################################################################################################################################
## NAME:	server.R
## PROJECT: Species Distribution Modelling
## DESC:	This is the server side of R shiny app -- it recomputes model every time "actionButton" in ui.R is clicked.
## AUTHOR:	Yugarshi Mondal
##
## INPUTS:	Static Data (should be in same folder)
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


## Static Final Global
current_vars <- load.predictors("./data/California/modern")
holocene_vars <-load.predictors("./data/California/midH")
lgm_vars<-load.predictors("./data/California/lgm")

shinyServer(function(input, output){

	# create a reactive data store
	model <- reactiveValues(data = NULL) # used for competing reactive button events
	coords <- reactiveValues(data = NULL) # used as a continually set global
	isDemo <- reactiveValues(data = NULL) # used as a continually set global

	# reacts to demo button
	observeEvent(input$demo, {
		isDemo$data <- 1
		temp <- read.csv('./data/tsuga.csv')
		coords$data <- temp[,1:2]

		# build a model
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Building Model", value = NULL)

   		data.ready <- prep.species(coords$data, current_vars, nb.absences=10000)
		model$data <- gbm.step(data.ready, 1:4, 'pres', tree.complexity=as.numeric(input$trDepth), learning.rate=as.numeric(input$lRate), max.trees=as.numeric(input$maxTrees), bag.fraction=as.numeric(input$bagFrac))

	})


	# This function "reacts" to project button
	observeEvent(input$project, {

		# Return Different Data Sets for Different Types of Computations
		if(!is.null(input$customPresAbs)) {
			appendData <- read.csv(input$customPresAbs$datapath)
			coords$data <- appendData[,1:2]
			isDemo$data <- 0
		} else if (is.null(input$customPresAbs)) {
			return()
		}


		# build a model
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Building Model", value = NULL)

   		data.ready <- prep.species(coords$data, current_vars, nb.absences=10000)
		model$data <- gbm.step(data.ready, 1:4, 'pres', tree.complexity=as.numeric(input$trDepth), learning.rate=as.numeric(input$lRate), max.trees=as.numeric(input$maxTrees), bag.fraction=as.numeric(input$bagFrac))

	})

	# Display model curve
	output$modelDiag <- renderPlot({
		if (is.null(model$data)) return()
		x <- model$data$trees.fitted
		y <- model$data$cv.values
		plot(x,y,type = 'l',xlab = 'number of trees', ylab = 'cv deviance',main = "Crossvalidated Boosting Diagnostic")
	})

	# Print model Diagnostics
	output$modelCVStats <- renderPrint({
		if (is.null(model$data)) return()
		return(model$data$cv.statistics)
	})
	output$numTrees <- renderPrint({
		if (is.null(model$data)) return()
		return(model$data$n.trees)
	})

	# Plot Projections
	output$modern <- renderPlot({
		if (is.null(model$data)) return()

		# Display progress
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Projecting", detail = "Modern", value = NULL)

    	# Reclassify
    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
    	# Use model to figure out distribution
		modern = predict(current_vars, model$data, n.trees=model$data$gbm.call$best.trees, type='response')
		# Reclassify probability of occurance to presense and absence
		modernBinary <- reclassify(modern, binaryReclass)
		# Plot projections
		plot(modernBinary, legend=FALSE, main="Modern Projection", xlab = "Longtitude", ylab = "Latitude")
		points(coords$data)
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		if (isDemo$data == 1 ){
			temp <- read.csv('./data/test.csv')
			points(temp, col = 'red', pch = 4)
		}

	})

	output$midH <- renderPlot({
		if (is.null(model$data)) return()

		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "Mid-Holocene", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		holocene = predict(holocene_vars, model$data, n.trees=model$data$gbm.call$best.trees, type='response')
		holoceneBinary <- reclassify(holocene, binaryReclass)
		plot(holoceneBinary, legend=FALSE, main="mid-Holocene Projection", xlab = "Longtitude", ylab = "Latitude")
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		if (isDemo$data == 1){
			points(read.csv('./data/test.csv'), col = 'red', pch = 4)
		}
	})

	output$lgm <- renderPlot({
		if (is.null(model$data)) return()

		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "Last Glacial Max", value = NULL)

    	binaryReclass <- matrix(c(0, as.numeric(input$thresh), 0, as.numeric(input$thresh), 1, 1), byrow=TRUE, ncol=3)
		lgm = predict(lgm_vars, model$data, n.trees=model$data$gbm.call$best.trees, type='response')
		lgmBinary <- reclassify(lgm, binaryReclass)
		plot(lgmBinary, legend=FALSE, main="LGM Projection", xlab = "Longtitude", ylab = "Latitude")
		if (!is.null(input$coPlot)){
			points(read.csv(input$coPlot$datapath), col = 'red', pch = 4)
		}
		if (isDemo$data == 1){
			points(read.csv('./data/test.csv'), col = 'red', pch = 4)
		}
	})

})
