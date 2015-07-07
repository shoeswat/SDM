library(shiny)
#requireNamespace('rgeos')
require(rgeos)
require(plyr)
require(ggplot2)
require(raster)
require(maptools)
require(grid)
library(RCurl)
source('SDM_library.R')

current_vars <- load.predictors(1)
holocene_vars <-load.predictors(2)
lgm_vars<-load.predictors(3)

##for reclassifying raster to 40% prob of pres
binaryReclass <- matrix(c(0, 0.4, 0, 0.4, 1, 1), byrow=TRUE, ncol=3)

#Tsuga heterophylla

shinyServer(function(input, output){
	
	model <- reactive({
		coords <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=300&georeferenced=True&format=csv")))
		coords <- coords[,1:2]

		#prep inputs/outputs; build model
		data.ready <- prep.species(coords[,1:2], current_vars, nb.absences=10000)
		gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75)
	})

	##to see the results
	output$modern <- renderPlot({
		modern = predict(current_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
		modernBinary <- reclassify(modern, binaryReclass)
		plot(modernBinary)
		#points(coords)
	})

})