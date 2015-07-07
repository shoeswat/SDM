library(shiny)

shinyUI(fluidPage(

	#Application Title
	titlePanel("Species Distribution Modeling using Boosted Regression Trees"),

	sidebarLayout(

		sidebarPanel(
			textInput("species", "Species:"),

			submitButton("Update View")
		),

		mainPanel(
			#tableOutput("view")
			plotOutput("modern")
		)

	)
))