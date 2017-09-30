
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
	HTML("There exists SNPs that give various pre-dispositions for fitness-levels. While the these power-vs-endurance variants are clearly reproducible throughout many studies, their effect is still rather limited - meaning that one can be a world-class sprinter without necessarily having 'the sprinter gene'. Many tests are sold for these specific SNPs, giving you knowledge of handful of specific fitness related SNPs. Many tests are over-sold. And of course the very same information is also available for free if you already have imputed genotyping microarray information. 

Here I have tried to collect a set of SNPs that clearly are of interest to know if you are into athletics"),
	
	endPanel(),
	htmlOutput("text1"),
	dataTableOutput("table1"),

	
	htmlOutput("text2"),
	dataTableOutput("table2"),

	# htmlOutput("text3"),
	# dataTableOutput("table3"),
	
	endPage(),
	footer()

	)	
)







