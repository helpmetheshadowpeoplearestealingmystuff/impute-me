


shinyUI(fluidPage(
	titlePanel("Analyse genome"),
	sidebarLayout(
		sidebarPanel(
			fileInput("largeFile", "First upload 23andme data", multiple = FALSE, accept = NULL),
			p("Then start imputation. This will take a while, but we'll mail you the results"),
			textInput(inputId="email", label = "email", value = ""),
			actionButton("goButton","Start imputation"),
			
			
			
			),
		mainPanel(
			textOutput("text1"),
			textOutput("text2")
			
			
		)
	)
))