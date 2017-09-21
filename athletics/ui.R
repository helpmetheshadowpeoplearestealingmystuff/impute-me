
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Athletics related SNPs"),
	beginPage(),
	beginPanel('1/3'),
	HTML("<i>Note, I have marked this module as a <b>legacy</b> module. That's not because it is wrong, merely because the content is somewhat information-poor.</i><br><br><br><br> Example ID to test: id_613z86871"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	HTML("There exists SNPs that give various pre-dispositions for fitness-levels. While the these power-vs-endurance variants are clearly reproducible throughout many studies, their effect is still rather limited - meaning that one can be a world-class sprinter without necessarily having 'the sprinter gene'. Many tests are sold for these specific SNPs, giving you knowledge of handful of specific fitness related SNPs. Of course the very same information is also available for free if you already have imputed genotyping microarray information. 

Provided in this module is therefore an overview of your fitness related SNPs. For further information on each, we refer to the litterature detailing each of the supporting information pieces."),
	
	endPanel(),
	dataTableOutput("table1"),
	# plotOutput("plot1")
	
	endPage(),
	footer()

	)	
)







