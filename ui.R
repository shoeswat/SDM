#################################################################################################################################
## NAME:	ui.R
## PROJECT: Species Distribution Modelling
## DESC:	This is the UI code for the R shiny app.
## AUTHOR:	Yugarshi Mondal
##
## INPUTS:	(none)
## RETURNS:	(null)
## REQ:		Should be in same directory with server.R and SDM_library.R
##

library(shiny)

shinyUI(fluidPage(

	titlePanel("Species Distribution Modeling using Boosted Regression Trees"),

	sidebarLayout(

		sidebarPanel(

			textInput("species", "Species Presence-Absense (search and/or upload)","e.g. Tsuga heterophylla"),

			fileInput('customPresAbs',NULL,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
            ),

            fileInput('coPlot',"Co-Plot Additional Points (e.g. other observations, study sites, etc)",
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )
            ),


			tags$h5("Model Parameters:"),

			div(id="latNorth_div",textInput(inputId="trDepth", label="Tree Depth", value = 3)),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#trDepth {max-width: 80px}")),

			div(id="latSouth_div",textInput(inputId="lRate", label="Learrning Rate", value = 0.05)),
			tags$head(tags$style(type="text/css", "#latSouth_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#lRate {max-width: 80px}")),

			div(id="lonEast_div",textInput(inputId="maxTrees", label="Max Trees", value = 100000000)),
			tags$head(tags$style(type="text/css", "#lonEast_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#maxTrees {max-width: 80px}")),

			div(id="lonWest_div",textInput(inputId="bagFrac", label="Bag Frac", value = .75)),
			tags$head(tags$style(type="text/css", "#lonWest_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#bagFrac {max-width: 80px}")),

			tags$h5("Plotting Parameters:"),

			div(id="latNorth_div",textInput(inputId="thresh", label="Presence-Absense Thresh", value = .4)),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#thresh {max-width: 80px; }")),

			tags$br(),

			actionButton("project","Project"),

			helpText("Troubleshooting: ", tags$br(), "1. If species can't be found, use ",
				tags$a(href="https://holos.berkeley.edu/explore/#&q=&page_size=100", "Holos"),
				"to verify that observation data exists.", tags$br(),
				"2. Computations will take some time; Status bar appears at top right of page."
			),

			tags$br(),

			helpText("Data Sources: ", tags$br(), "Species Range Data:",
				tags$a(href="https://ecoengine.berkeley.edu/", "Berkeley Ecoengine API"), tags$br(),
				"Cliamte Data: Mondal et. al., 2015 (in review)",tags$br(),"Model: gbm.step (dismo R package)"
			),

			helpText("Devs: Yugarshi Mondal, Scott Farley",tags$br(),"PIs: Roger Byrne, Dave Wahl")
		),




		mainPanel(
			tags$h4("Projections"),
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

			),

			tags$h4("Model Diagnostics"),
			fluidRow(
				column(6,
					verbatimTextOutput("modelCVStats")
				),
				column(6 ,
					plotOutput("modelDiag"),
					verbatimTextOutput("numTrees")
				)
			)

		)

	)
))