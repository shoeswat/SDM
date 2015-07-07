library(shiny)
#install.packages("devtools")
#devtools::install_github("shiny-incubator", "rstudio")
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

	observe({

		if (input$save == 0)
	    	return()

		isolate({

			coords <- reactive({
				dummy <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=300&georeferenced=True&format=csv")))
				dummy[,1:2]
			})
				
			model <- reactive({
				progress <- shiny::Progress$new()
			    on.exit(progress$close())
		    	progress$set(message = "Building Model", value = NULL)

				data.ready <- prep.species(coords(), current_vars, nb.absences=10000)
				gbm.step(data.ready, 1:19, 'pres', tree.complexity=3, learning.rate=0.05, max.trees=100000000, bag.fraction=0.75)
			})

			##to see the results
			output$modern <- renderPlot({
				modern = predict(current_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')

				progress <- shiny::Progress$new()
			    on.exit(progress$close())
		    	progress$set(message = "Projecting", value = NULL)

				modernBinary <- reclassify(modern, binaryReclass)
				plot(modernBinary)
				points(coords())
			})

			output$midH <- renderPlot({
				progress <- shiny::Progress$new()
			    on.exit(progress$close())
		    	progress$set(message = "Projecting", detail = "mid-Holocene", value = NULL)

				holocene = predict(holocene_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
				holoceneBinary <- reclassify(holocene, binaryReclass)
				plot(holoceneBinary)
			})

			output$lgm <- renderPlot({
				progress <- shiny::Progress$new()
			    on.exit(progress$close())
		    	progress$set(message = "Projecting", detail = "Last Glacial Max", value = NULL)

				lgm = predict(lgm_vars, model(), n.trees=model()$gbm.call$best.trees, type='response')
				lgmBinary <- reclassify(lgm, binaryReclass)
				plot(lgmBinary)
			})
	  	})
	})

})