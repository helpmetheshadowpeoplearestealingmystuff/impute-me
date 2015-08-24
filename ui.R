


shinyUI(fluidPage(
	titlePanel("Analyse genome"),
	sidebarLayout(
		sidebarPanel(
			fileInput("largeFile", "First upload 23andme data", multiple = FALSE, accept = NULL),
			textInput(inputId="email", label = "Then give your email", value = "lassefolkersen@gmail.com"),
			p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
			actionButton("goButton","Start imputation"),
			width=4
			
			
			
			),
		mainPanel(
			textOutput("text1"),
			textOutput("text2"),
			textOutput("text3")
			
			
		)
	)
))