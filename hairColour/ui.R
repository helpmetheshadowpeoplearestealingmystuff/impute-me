
source("../uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Guess my hair colour"),
	beginPage(),	
	beginPanel('1/3'),
	#Beginning of page elements

	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
	
	checkboxInput("col_provided", label ="Provide own hair colour", value = FALSE),
	conditionalPanel(
		condition = "input.col_provided",
		sliderInput("blondeness", "Blondeness",min=0, max=100, value=50),
		sliderInput("redheadness", "Red-headness",min=0, max=100, value=0)
	),
	
	actionButton("goButton","Run analysis"),

	endPanel(),
	beginPanel('2/3'),

	htmlOutput("text1"),

	endPanel(),
	beginPanel(),

	plotOutput("haircol1"),
	# textOutput("text2"),
	# plotOutput("haircol2")

	endPanel(),
	#End of page elements
	endPage(),
	footer()
	)
)






