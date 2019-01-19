
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Rare diseases"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This module analyses the 'inherited conditions' from the 2016 version of 23andme health. <br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br><br>"),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	HTML("<br><br><br><br><br><br>"),
	endPanel(),
	beginPanel('2/3'),
	
	
	endPanel(),
	htmlOutput("text1"),
	htmlOutput("text2"),
	HTML("<br><br><br>"),
	dataTableOutput("table1"),
	
	endPage(),
	footer()
	
)	
)







