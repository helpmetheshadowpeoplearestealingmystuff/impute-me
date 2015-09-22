


shinyUI(fluidPage(
	titlePanel("Guess my hair colour"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_1l431dI24"),
			# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
			
			checkboxInput("col_provided", label ="Provide own hair colour", value = FALSE),
			conditionalPanel(
				condition = "input.col_provided",
				sliderInput("blondeness", "Blondeness",min=0, max=100, value=50),
				sliderInput("redheadness", "Red-headness",min=0, max=100, value=0)
			),
			
			actionButton("goButton","Run analysis"),
			width=4
			
			
			
		),
		mainPanel(
			plotOutput("haircol1"),
			htmlOutput("text1")
			# textOutput("text2"),
			# plotOutput("haircol2")
			
			
		)
	)
))






