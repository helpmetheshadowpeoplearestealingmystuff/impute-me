
source("../uifunctions.R")
initialize('sti',TRUE)


weights <- paste0("w",0:7)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("LDpred based MDD scores"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This module includes the LD-pred based scores calculated by A Ingason"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("weighting_scheme", "Weighting scheme:", choices = weights),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	tableOutput("table1"), #getting the table created in server.R

	endPanel(),
	

	endPage(),
	footer()
))






