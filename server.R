## Global Libraries
library(shiny)
require(rgeos)
require(plyr)
require(ggplot2)
require(raster)
require(maptools)
require(grid)
library(RCurl)
source('SDM_library.R')

## Load Data Stack
current_vars <- load.predictors(1)
holocene_vars <-load.predictors(2)
lgm_vars<-load.predictors(3)

##for reclassifying raster to 40% prob of pres
binaryReclass <- matrix(c(0, 0.4, 0, 0.4, 1, 1), byrow=TRUE, ncol=3)

#Tsuga heterophylla

shinyServer(function(input, output){

	# When "project" button pushed, send search request to API
	coords <- eventReactive(input$project, {
		dummy <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=300&georeferenced=True&format=csv")))
		dummy[,1:2]
	})
		
	# Build a model from the response of the API (which should be presense points of species)
	model <- reactive({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
    	progress$set(message = "Building Model", value = NULL)

		data.ready <- prep.species(coords(), current_vars, nb.absences=10000)
		gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75)
	})

	# Display model curve
	output$modelDiag <- renderPlot({
		model()
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

    	# Use model to figure out distribution
		modern = predict(current_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		# Reclassify probability of occurance to presense and absence
		modernBinary <- reclassify(modern, binaryReclass)
		# Plot projections
		plot(modernBinary, legend=FALSE, main="Modern Projection", xlab = "Longtitude", ylab = "Latitude")
		points(coords())
	})

	output$midH <- renderPlot({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "mid-Holocene", value = NULL)

		holocene = predict(holocene_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		holoceneBinary <- reclassify(holocene, binaryReclass)
		plot(holoceneBinary, legend=FALSE, main="mid-Holocene Projection", xlab = "Longtitude", ylab = "Latitude")
	})

	output$lgm <- renderPlot({
		progress <- shiny::Progress$new()
	    on.exit(progress$close())
	   	progress$set(message = "Projecting", detail = "Last Glacial Max", value = NULL)

		lgm = predict(lgm_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		lgmBinary <- reclassify(lgm, binaryReclass)
		plot(lgmBinary, legend=FALSE, main="LGM Projection", xlab = "Longtitude", ylab = "Latitude")
	})

})