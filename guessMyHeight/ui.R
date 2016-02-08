
source("../uifunctions.R")
initialize('gmh',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Apperance"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Appearance traits, such as height and hair colour are highly heritable and your genome can provide clues to them. Of course the best source for this is a mirror. <br><br>Nonetheless it can be  interesting to perform the analysis on e.g. children to provide estimates of their final height and apperance. Other traits such as eye and skin colour and even facial form, may be provided in the future. However, the fact is that these traits are much harder to predict and therefore genetic analysis provides more limited benefit. This module therefore focuses on height, which is highly heritable, and hair-colour which currently is a bit more guess-work.<br><br>To run analysis input your user-id, or use the test-value of id_57n662948:<br><br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
	checkboxInput("height_provided", label ="Provide own height", value = FALSE),
	
	conditionalPanel(
		condition = "input.height_provided",
		textInput("real_height", "Your real height (cm)",value=""),
		textInput("real_age", "Your age (years)",value="")
	),
	checkboxInput("col_provided", label ="Provide own hair colour", value = FALSE),
	conditionalPanel(
		condition = "input.col_provided",
		sliderInput("blondeness", "Blondeness",min=0, max=100, value=50),
		sliderInput("redheadness", "Red-headness",min=0, max=100, value=0)
	),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	h2("Height estimate"),
	htmlOutput("text_height1"),
	plotOutput("plot_height1"),
	htmlOutput("text_height2"),
	
	HTML("<br><br><br>"),
	h2("Hair colour estimate"),
	htmlOutput("text_haircol1"),
	plotOutput("plot_haircol1"),
	htmlOutput("text_haircol2"),

	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







