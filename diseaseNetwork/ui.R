
source("../uifunctions.R")
initialize('sti',TRUE)

library("igraph")
library("visNetwork")


shinyUI(bootstrapPage(
	head(),
	navigation(),

	titlePanel("Disease Networks"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This is a module that can visualize the entire compendium of human disease - at each point showing relevant genetic findings. The goal is to illustrate how to present genetic data <i>depending</i> on a medical status.<br><br>Use mouse-scroll-button to zoom, mouse-over for full names, and mouse-click for navigating through the network. In each coloured circle an ID-code for a genetic score is found, further detailed in table on click. The test ID is <i>id_613z86871</i>.<br><br><br>"),

	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Go"),

	endPanel(),
	beginPanel('2/3'),
	visNetworkOutput("plot1"),
	tableOutput("table1"),	
	htmlOutput("text_1"),
	endPanel(),
	
	
	endPage(),
	footer()
))






