#################################################################################################################################
## NAME:	ui.R
## PROJECT: Species Distribution Modelling
## DESC:	This is the UI code for the R shiny app.
## AUTHOR:	Scott Farley
##
## INPUTS:	(none)
## RETURNS:	(null)
## REQ:		Should be in same directory with server.R and SDM_library.R
##

library(shiny)

shinyUI(fluidPage(

	#Application Title
	titlePanel("Species Distribution Modeling using Boosted Regression Trees"),

	sidebarLayout(

		sidebarPanel(

			textInput("species", "Species:"),

			actionButton("project","Project"),

			helpText("If species cannot be found, use ",
				tags$a(href="https://holos.berkeley.edu/explore/#&q=&page_size=100", "Holos"),
				"to verify that observation data exists.",tags$br(),"Note: Computations Will Take Some Time"
			),

			tags$br(),

			tableOutput("modelCVStats"),

			tags$br(),

			plotOutput("modelDiag"),

			helpText("Data Sources: ", tags$br(), "Species Range Data:",
				tags$a(href="https://ecoengine.berkeley.edu/", "Berkeley Ecoengine API"), tags$br(),
				"Cliamte Data: Mondal et. al., 2015 (in review)",tags$br(),"Model: gbm.step (dismo R package)"
			),

			helpText("Devs: Yugarshi Mondal, Scott Farley",tags$br(),"PIs: Roger Byrn, Dave Wahl")
		),




		mainPanel(
			fluidRow(
				column(4,
					plotOutput("modern")
				),
				column(4,
					plotOutput("midH")
				),
				column(4,
					plotOutput("lgm")
				)				

			)
		)

	)
))