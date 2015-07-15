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

			textInput("species", "Species (search data or upload)","e.g. Tsuga heterophylla"),

			fileInput('customPresAbs',NULL,
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv',
                  '.tsv',
                  'text/plain',
                  'text/tab-separated-values'
                )
            ),

			radioButtons('computationType', 'Computation Type:',
				c("Project using presence from Ecoengine & Upload Data (if it exists)"=1,
                  "Use Ecoengine to Project, Overlay Custom Presence Data"=2,
                  "Project using only Upload Data"=3)
			),


			div(id="latNorth_div",textInput(inputId="latNorth", label="Lat Noth", value = 43.0456)),
			tags$head(tags$style(type="text/css", "#latNorth_div {display: inline-block; text-align: center; }")),
			tags$head(tags$style(type="text/css", "#latNorth {max-width: 100px}")),

			div(id="latSouth_div",textInput(inputId="latSouth", label="Lat South", value = 42.0345)),
			tags$head(tags$style(type="text/css", "#latSouth_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#latSouth {max-width: 100px}")),

			div(id="lonEast_div",textInput(inputId="lonEast", label="Lon East", value = -120.0530)),
			tags$head(tags$style(type="text/css", "#lonEast_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#lonEast {max-width: 100px}")),

			div(id="lonWest_div",textInput(inputId="lonWest", label="Lon West", value = -123.0638)),
			tags$head(tags$style(type="text/css", "#lonWest_div {display: inline-block; text-align: center;}")),
			tags$head(tags$style(type="text/css", "#lonWest {max-width: 100px}")),

			tags$br(),

			actionButton("project","Project"),

			helpText("Troubleshooting: ", tags$br(), "1. If species can't be found, use ",
				tags$a(href="https://holos.berkeley.edu/explore/#&q=&page_size=100", "Holos"),
				"to verify that observation data exists.", tags$br(),
				"2. If no input range is given, California will be plotted", tags$br(),
				"3. Computations will take some time; Status bar appears at top right of page."
			),

			tags$br(),

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

			),

			fluidRow(
				column(6,
					tableOutput("modelCVStats")
				),
				column(6,
					plotOutput("modelDiag")
				)
			)

		)

	)
))