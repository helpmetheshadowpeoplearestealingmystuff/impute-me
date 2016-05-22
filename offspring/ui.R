
source("../uifunctions.R")
initialize('sti',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("template"),
	beginPage(),
	beginPanel('1/3'),
	HTML("Some text that will go above the input ID place"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	h2("Some title"),
	HTML("Some longer text describing what you do"),
	tableOutput("table1"), #getting the table created in server.R

	endPanel(),
	
	# Optional alternative for when you want to show a plot
	# plotOutput("plot1")
	
	endPage(),
	footer()
))






