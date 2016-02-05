
source("../uifunctions.R")
initialize('gmh',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Guess my height"),
	beginPage(),	
	beginPanel('1/3'),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
	checkboxInput("height_provided", label ="Provide own height", value = FALSE),
	conditionalPanel(
		condition = "input.height_provided",
		textInput("real_height", "Your real height (cm)",value=""),
		textInput("real_age", "Your age (years)",value="")
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	htmlOutput("text1"),
	endPanel(),
	beginPanel(),
	# textOutput("text2"),
	plotOutput("plot1"),
	endPanel(),
	endPage(),
	footer()
		
	)
)







