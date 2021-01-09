
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Kandinskyfy your Genome"),
	beginPage(),
	beginPanel('1/3'),
	HTML("To run analysis input your user-id, or use the test-value of id_613z86871:"),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	downloadButton("downloadData", label = "Download pdf"),
	endPanel(),
	beginPanel('2/3'),
	
	
	
	htmlOutput("text_3"),
	
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	
	endPanel(),
	endPage(),
	footer()
	
)	
)







