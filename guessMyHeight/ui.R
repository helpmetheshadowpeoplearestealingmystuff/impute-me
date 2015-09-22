


shinyUI(fluidPage(
	titlePanel("Guess my height"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_1l431dI24"),
			# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
			checkboxInput("height_provided", label ="Provide own height", value = FALSE),
			conditionalPanel(
				condition = "input.height_provided",
				textInput("real_height", "Your real height (cm)",value=""),
				textInput("real_age", "Your age (years)",value="")
			),
			
			actionButton("goButton","Run analysis"),
			width=4
			
			
			
		),
		mainPanel(
			htmlOutput("text1"),
			# textOutput("text2"),
			plotOutput("plot1")
			
			
		)
	)
))






