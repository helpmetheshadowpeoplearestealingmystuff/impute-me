
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Athletics related SNPs"),
	beginPage(),
	beginPanel('1/3'),
	HTML("<br><br> Example ID to test: id_613z86871"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  checkboxInput("source_notes", label ="Show source notes for risk score", value = FALSE)
	),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),

	htmlOutput("text1"),
	DT::dataTableOutput("table1"),

	
	htmlOutput("text2"),
	DT::dataTableOutput("table2"),

	# htmlOutput("text3"),
	# DT::dataTableOutput("table3"),
	endPanel(),
	
	endPage(),
	footer()

	)	
)







