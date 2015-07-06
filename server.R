library(shiny)
library(RCurl)
source('SDM_library.R')

#Tsuga heterophylla

shinyServer(function(input, output){
	
	dummy <- reactive({
		coords <- read.csv(text = getURL(paste0("https://ecoengine.berkeley.edu/api/observations/?q=scientific_name:%22",URLencode(input$species),"%22&fields=geojson&page_size=300&georeferenced=True&format=csv")))
		coords <- coords[,1:2]
	})

	output$view <- renderTable({
		head(dummy())
	})

})