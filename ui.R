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

	titlePanel("Species Distribution Modeling in California using Boosted Regression Trees"),

	sidebarLayout(

		sidebarPanel(

			fileInput('customPresAbs',"Species Presence-Absense Upload",
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

			helpText(
				"Format: Uploads must be in CSV format -- Longitude in 1st column and Latitude in the 2nd."
			),

			tags$br(),

			tags$h5("Model Parameters:"),

			div(id="latNorth_div",textInput(inputId="trDepth", label="Tree Depth", value = 3)),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#trDepth {max-width: 80px}")),

			div(id="latSouth_div",textInput(inputId="lRate", label="Lrn Rate", value = 0.05)),
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

			actionButton("project","Project", class = "btn-warning"),

			tags$hr(),
			
			div(id="latNorth_div",tags$h5("Try demo projection:")),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#thresh {max-width: 80px; }")),

			div(id="latNorth_div",actionButton("demo","Demo")),

			div(id="latNorth_div",tags$h5("(Tsuga heterophylla w/ Pollen Samples)")),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#thresh {max-width: 80px; }")),

			tags$br(),

			helpText(
				"Black Circles are Presence-Absense points off of which model is built. The red crosses are co-plotted points."
			),

			helpText("Data Sources: ", tags$br(), "Species Range Data:",
				tags$a(href="https://ecoengine.berkeley.edu/", "Berkeley Ecoengine API"), tags$br(),
				"Cliamte Data: Mondal et. al., 2015 (in review)",tags$br(),"Model: gbm.step (dismo R package)"
			),

			helpText("Devs: Yugarshi Mondal",tags$br(),"PIs: Roger Byrne, Dave Wahl")
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