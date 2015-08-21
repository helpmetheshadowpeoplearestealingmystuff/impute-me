


shinyUI(fluidPage(
	titlePanel("Analyse genome"),
	sidebarLayout(
		sidebarPanel(
			fileInput("largeFile", "First upload 23andme data", multiple = FALSE, accept = NULL),
			p("Then start imputation (will take a while)"),
			actionButton("goButton","Start imputation")
			
			),
		mainPanel(
			textOutput("text1"),
			textOutput("text2"),
			p("hello")
			
		)
	)
))