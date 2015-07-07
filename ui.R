library(shiny)

shinyUI(fluidPage(

	#Application Title
	titlePanel("Species Distribution Modeling using Boosted Regression Trees"),

	sidebarLayout(

		sidebarPanel(
			textInput("species", "Species:"),

			actionButton("save","Project")

			#width = 2
		),

		mainPanel(
			#tableOutput("view")
			plotOutput("modern"),
			plotOutput("midH"),
			plotOutput("lgm")
			#width = 9
		)

	)
))