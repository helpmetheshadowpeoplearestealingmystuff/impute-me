
source("../uifunctions.R")
initialize('sti',TRUE)
library(plotly)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Ethnicity and race"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This browser investigates the ethnicity profile"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	h2("Plotly 3D plot of genotype principal components"),
	
	endPanel(),
	
	# Optional alternative for when you want to show a plot
	plotlyOutput("mainPlot"),
	
	endPage(),
	footer()
))






